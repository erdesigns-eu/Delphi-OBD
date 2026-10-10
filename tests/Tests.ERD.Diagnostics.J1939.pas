//------------------------------------------------------------------------------
//  Tests.ERD.Diagnostics.J1939
//
//  Coverage for the J1939-diagnostic components:
//    - TOBDJ1939     (bus client — NAME, address claim, request)
//    - TOBDJ1939DM   (selected DM1..DM31 decoder)
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial fixture.
//------------------------------------------------------------------------------

unit Tests.ERD.Diagnostics.J1939;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF},
  {$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF},
  DUnitX.TestFramework,
  ERD.Errors,
  ERD.Types,
  ERD.Protocol.J1939,
  ERD.Diagnostics.J1939,
  ERD.Diagnostics.J1939.DM,
  ERD.Diagnostics.J1939.DM.DM;

type
  TTestCallback1 = reference to procedure(Sender: TObject; APriority: Byte; APGN: Cardinal; ASA: Byte; ADA: Byte; const AData: TBytes);
  TTestCallback2 = reference to procedure(Sender: TObject; APGN: Cardinal; const ALamps: TOBDJ1939Lamps; const AEntries: TArray<TOBDJ1939DtcEntry>);
  TTestCallback3 = reference to procedure(Sender: TObject; APGN: Cardinal; const AData: TBytes);
  TTestCallback4 = reference to procedure(Sender: TObject; APGN: Cardinal; const ALamps: TOBDJ1939Lamps; const AEntries: TArray<TOBDJ1939DtcEntry>);

  /// <summary>
  ///   DUnitX fixture for the J1939 components.
  /// </summary>
  [TestFixture]
  TJ1939DiagnosticsTests = class
  strict private
    FTestCallback1: TTestCallback1;
    FTestCallback2: TTestCallback2;
    FTestCallback3: TTestCallback3;
    FTestCallback4: TTestCallback4;
    procedure HandleTestEvent1(Sender: TObject; APriority: Byte; APGN: Cardinal; ASA: Byte; ADA: Byte; const AData: TBytes);
    procedure HandleTestEvent2(Sender: TObject; APGN: Cardinal; const ALamps: TOBDJ1939Lamps; const AEntries: TArray<TOBDJ1939DtcEntry>);
    procedure HandleTestEvent3(Sender: TObject; APGN: Cardinal; const AData: TBytes);
    procedure HandleTestEvent4(Sender: TObject; APGN: Cardinal; const ALamps: TOBDJ1939Lamps; const AEntries: TArray<TOBDJ1939DtcEntry>);

  public
    [Test] procedure J1939_DefaultsSourceIsNull;
    [Test] procedure J1939_SetNAMECopiesAndPads;
    [Test] procedure J1939_NAMEIndexOutOfRangeRaises;
    [Test] procedure J1939_BuildClaimedPayloadIsNAME;
    [Test] procedure J1939_BuildRequestPayloadIsLE3Bytes;
    [Test] procedure J1939_EncodeIdMatchesCodec;
    [Test] procedure J1939_DispatchInboundFiresOnFrameForNonTP;

    [Test] procedure DM_DecodeEntrySPNAndFMI;
    [Test] procedure DM_DecodeEntryCMAndOC;
    [Test] procedure DM_DispatchDM1FiresStructured;
    [Test] procedure DM_DispatchDM3FiresRaw;
    [Test] procedure DM_DM1AllZeroDtcSkipped;
  end;

implementation

{ ---- TOBDJ1939 ---------------------------------------------------------- }

procedure TJ1939DiagnosticsTests.J1939_DefaultsSourceIsNull;
var
  J: TOBDJ1939;
begin
  J := TOBDJ1939.Create(nil);
  try
    Assert.AreEqual(Integer(J1939_NULL_ADDRESS),
      Integer(J.SourceAddress));
    Assert.IsNotNull(J.Sessions);
  finally
    J.Free;
  end;
end;

procedure TJ1939DiagnosticsTests.J1939_SetNAMECopiesAndPads;
var
  J: TOBDJ1939;
begin
  J := TOBDJ1939.Create(nil);
  try
    J.SetNAME([$11, $22, $33]);
    Assert.AreEqual($11, Integer(J.NAME[0]));
    Assert.AreEqual($22, Integer(J.NAME[1]));
    Assert.AreEqual($33, Integer(J.NAME[2]));
    Assert.AreEqual($00, Integer(J.NAME[3]));
    Assert.AreEqual($00, Integer(J.NAME[7]));
  finally
    J.Free;
  end;
end;

procedure TJ1939DiagnosticsTests.J1939_NAMEIndexOutOfRangeRaises;
var
  J: TOBDJ1939;
begin
  J := TOBDJ1939.Create(nil);
  try
    Assert.WillRaise(
      procedure
      var
        Dummy: Byte;
      begin
        Dummy := J.NAME[8];
        Dummy := Dummy;
      end,
      EOBDConfig);
  finally
    J.Free;
  end;
end;

procedure TJ1939DiagnosticsTests.J1939_BuildClaimedPayloadIsNAME;
var
  J: TOBDJ1939;
  Body: TBytes;
begin
  J := TOBDJ1939.Create(nil);
  try
    J.SetNAME([$01, $02, $03, $04, $05, $06, $07, $08]);
    Body := J.BuildAddressClaimedPayload;
    Assert.AreEqual(8, Length(Body));
    Assert.AreEqual($01, Integer(Body[0]));
    Assert.AreEqual($08, Integer(Body[7]));
  finally
    J.Free;
  end;
end;

procedure TJ1939DiagnosticsTests.J1939_BuildRequestPayloadIsLE3Bytes;
var
  J: TOBDJ1939;
  Body: TBytes;
begin
  J := TOBDJ1939.Create(nil);
  try
    Body := J.BuildRequestPayload(J1939_PGN_DM1);
    Assert.AreEqual(3, Length(Body));
    Assert.AreEqual(Integer(J1939_PGN_DM1 and $FF), Integer(Body[0]));
    Assert.AreEqual(Integer((J1939_PGN_DM1 shr 8) and $FF),
      Integer(Body[1]));
    Assert.AreEqual(Integer((J1939_PGN_DM1 shr 16) and $FF),
      Integer(Body[2]));
  finally
    J.Free;
  end;
end;

procedure TJ1939DiagnosticsTests.J1939_EncodeIdMatchesCodec;
var
  J: TOBDJ1939;
  IdViaComponent: Cardinal;
  IdViaCodec: Cardinal;
begin
  J := TOBDJ1939.Create(nil);
  try
    J.SourceAddress := $42;
    IdViaComponent := J.EncodeId(3, J1939_PGN_DM1, $FF);
    IdViaCodec := TOBDJ1939Codec.EncodeId(3, J1939_PGN_DM1, $42, $FF);
    Assert.AreEqual(Integer(IdViaCodec), Integer(IdViaComponent));
  finally
    J.Free;
  end;
end;

procedure TJ1939DiagnosticsTests.J1939_DispatchInboundFiresOnFrameForNonTP;
var
  J: TOBDJ1939;
  Hit: Integer;
  GotPGN: Cardinal;
  Id: Cardinal;
begin
  J := TOBDJ1939.Create(nil);
  try
    Hit := 0;
    GotPGN := 0;
    FTestCallback1 :=
      procedure(Sender: TObject; APriority: Byte; APGN: Cardinal;
        ASA: Byte; ADA: Byte; const AData: TBytes)
      begin
        Inc(Hit);
        GotPGN := APGN;
      end;
    J.OnFrame := HandleTestEvent1;
    // Build a CAN ID for a non-TP PGN (DM1) from source 0x42.
    Id := TOBDJ1939Codec.EncodeId(3, J1939_PGN_DM1, $42, $FF);
    J.DispatchInbound(Id,
      TBytes.Create($00, $FF, $00, $00, $00, $00, $00, $00));
    Assert.AreEqual(1, Hit);
    Assert.AreEqual(Integer(J1939_PGN_DM1), Integer(GotPGN));
  finally
    J.Free;
  end;
end;

{ ---- TOBDJ1939DM -------------------------------------------------------- }

procedure TJ1939DiagnosticsTests.DM_DecodeEntrySPNAndFMI;
var
  Buf: TBytes;
  E: TOBDJ1939DtcEntry;
begin
  // SPN = 0x12345 (74565), FMI = 3.
  //   byte 0 = SPN lo  = 0x45
  //   byte 1 = SPN mid = 0x23
  //   byte 2 = (SPN hi << 5) | FMI = (1 << 5) | 3 = 0x23
  //   byte 3 = 0 (CM=0, OC=0)
  Buf := TBytes.Create($45, $23, $23, $00);
  E := TOBDJ1939DM.DecodeEntry(Buf, 0);
  Assert.AreEqual(Integer($12345), Integer(E.SPN));
  Assert.AreEqual(3, Integer(E.FMI));
end;

procedure TJ1939DiagnosticsTests.DM_DecodeEntryCMAndOC;
var
  Buf: TBytes;
  E: TOBDJ1939DtcEntry;
begin
  // CM = 1, OC = 42 → byte 3 = 0x80 | 42 = 0xAA.
  Buf := TBytes.Create($00, $00, $00, $AA);
  E := TOBDJ1939DM.DecodeEntry(Buf, 0);
  Assert.AreEqual(1, Integer(E.CM));
  Assert.AreEqual(42, Integer(E.OC));
end;

procedure TJ1939DiagnosticsTests.DM_DispatchDM1FiresStructured;
var
  DM: TOBDJ1939DM;
  Hit: Integer;
  EntryCount: Integer;
  MIL: Boolean;
  Body: TBytes;
begin
  DM := TOBDJ1939DM.Create(nil);
  try
    Hit := 0;
    EntryCount := 0;
    MIL := False;
    FTestCallback2 :=
      procedure(Sender: TObject; APGN: Cardinal;
        const ALamps: TOBDJ1939Lamps;
        const AEntries: TArray<TOBDJ1939DtcEntry>)
      begin
        Inc(Hit);
        EntryCount := Length(AEntries);
        MIL := ALamps.MIL;
      end;
    DM.OnDTCs := HandleTestEvent2;
    // Lamps byte 0 = 0x40 (MIL on = 01 in bits 7..6), byte 1 = 0xFF.
    // One DTC record: SPN 0x100 (256), FMI 5.
    //   SPN lo = 0x00, mid = 0x01, byte2 = (0<<5) | 5 = 0x05, byte3 = 0x01.
    Body := TBytes.Create($40, $FF,
                          $00, $01, $05, $01);
    DM.DispatchDM(J1939_PGN_DM1, Body);
    Assert.AreEqual(1, Hit);
    Assert.AreEqual(1, EntryCount);
    Assert.IsTrue(MIL);
  finally
    DM.Free;
  end;
end;

procedure TJ1939DiagnosticsTests.DM_DispatchDM3FiresRaw;
var
  DM: TOBDJ1939DM;
  Hit: Integer;
begin
  DM := TOBDJ1939DM.Create(nil);
  try
    Hit := 0;
    FTestCallback3 :=
      procedure(Sender: TObject; APGN: Cardinal; const AData: TBytes)
      begin
        Inc(Hit);
      end;
    DM.OnRaw := HandleTestEvent3;
    DM.DispatchDM(J1939_PGN_DM3, TBytes.Create($AA, $BB));
    Assert.AreEqual(1, Hit);
  finally
    DM.Free;
  end;
end;

procedure TJ1939DiagnosticsTests.DM_DM1AllZeroDtcSkipped;
var
  DM: TOBDJ1939DM;
  EntryCount: Integer;
  Body: TBytes;
begin
  DM := TOBDJ1939DM.Create(nil);
  try
    EntryCount := -1;
    FTestCallback4 :=
      procedure(Sender: TObject; APGN: Cardinal;
        const ALamps: TOBDJ1939Lamps;
        const AEntries: TArray<TOBDJ1939DtcEntry>)
      begin
        EntryCount := Length(AEntries);
      end;
    DM.OnDTCs := HandleTestEvent4;
    // Lamps both zero + 4-byte all-zero DTC = "no fault".
    Body := TBytes.Create($00, $FF, $00, $00, $00, $00);
    DM.DispatchDM(J1939_PGN_DM1, Body);
    Assert.AreEqual(0, EntryCount);
  finally
    DM.Free;
  end;
end;

// Object-method adapters keep published events compatible with the IDE.
procedure TJ1939DiagnosticsTests.HandleTestEvent1(Sender: TObject; APriority: Byte; APGN: Cardinal; ASA: Byte; ADA: Byte; const AData: TBytes);
begin
  FTestCallback1(Sender, APriority, APGN, ASA, ADA, AData);
end;

procedure TJ1939DiagnosticsTests.HandleTestEvent2(Sender: TObject; APGN: Cardinal; const ALamps: TOBDJ1939Lamps; const AEntries: TArray<TOBDJ1939DtcEntry>);
begin
  FTestCallback2(Sender, APGN, ALamps, AEntries);
end;

procedure TJ1939DiagnosticsTests.HandleTestEvent3(Sender: TObject; APGN: Cardinal; const AData: TBytes);
begin
  FTestCallback3(Sender, APGN, AData);
end;

procedure TJ1939DiagnosticsTests.HandleTestEvent4(Sender: TObject; APGN: Cardinal; const ALamps: TOBDJ1939Lamps; const AEntries: TArray<TOBDJ1939DtcEntry>);
begin
  FTestCallback4(Sender, APGN, ALamps, AEntries);
end;

initialization
  TDUnitX.RegisterTestFixture(TJ1939DiagnosticsTests);

end.
