//------------------------------------------------------------------------------
//  Tests.OBD.Flashing.Transfer
//
//  Coverage for the UDS transfer engine: TOBDUDSTransfer safety gates + state machine
//  shape + cursor encoding for resume; TOBDJ1939MemoryAccess
//  encode / decode round-trips for DM14 / DM15 / DM16.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit Tests.OBD.Flashing.Transfer;

interface

uses
  System.SysUtils,
  System.Classes,
  DUnitX.TestFramework,
  OBD.Types,
  OBD.UDS.Transfer,
  OBD.J1939.MemoryAccess;

type
  /// <summary>Transfer engine safety + cursor coverage.</summary>
  [TestFixture]
  TUDSTransferTests = class
  public
    [Test] procedure RunRaisesWhenAutoExecuteFalse;
    [Test] procedure RunRaisesOnEmptyImage;
    [Test] procedure ResumeRaisesWhenCursorAlreadyComplete;
    [Test] procedure ResumeRaisesOnImageSizeMismatch;
    [Test] procedure InitialStateIsIdle;
    [Test] procedure RetryDefaults;
  end;

  /// <summary>J1939 memory-access framing coverage.</summary>
  [TestFixture]
  TJ1939MemoryAccessTests = class
  public
    [Test] procedure DM14RoundTrip;
    [Test] procedure DM14LengthOverflowRaises;
    [Test] procedure DM15RoundTrip;
    [Test] procedure DM16RoundTrip;
    [Test] procedure DM14CommandConstantsExposed;
  end;

implementation

{ ---- TOBDUDSTransfer ------------------------------------------------------- }

procedure TUDSTransferTests.RunRaisesWhenAutoExecuteFalse;
var
  T: TOBDUDSTransfer;
begin
  T := TOBDUDSTransfer.Create(nil);
  try
    Assert.IsFalse(T.AutoExecute);
    Assert.WillRaise(
      procedure begin T.Run($1000, TBytes.Create($00)); end,
      EOBDConfig);
  finally
    T.Free;
  end;
end;

procedure TUDSTransferTests.RunRaisesOnEmptyImage;
var
  T: TOBDUDSTransfer;
begin
  T := TOBDUDSTransfer.Create(nil);
  try
    T.AutoExecute := True;
    Assert.WillRaise(
      procedure begin T.Run($1000, nil); end,
      EOBDConfig);
  finally
    T.Free;
  end;
end;

procedure TUDSTransferTests.ResumeRaisesWhenCursorAlreadyComplete;
var
  T: TOBDUDSTransfer;
  Cursor: TOBDTransferCursor;
begin
  T := TOBDUDSTransfer.Create(nil);
  try
    T.AutoExecute := True;
    Cursor := Default(TOBDTransferCursor);
    Cursor.TotalBytes := 4;
    Cursor.BytesSent := 4;
    Cursor.MaxChunkBytes := 4;
    Assert.WillRaise(
      procedure begin
        T.Resume(Cursor, TBytes.Create($00, $01, $02, $03));
      end,
      EOBDConfig);
  finally
    T.Free;
  end;
end;

procedure TUDSTransferTests.ResumeRaisesOnImageSizeMismatch;
var
  T: TOBDUDSTransfer;
  Cursor: TOBDTransferCursor;
begin
  T := TOBDUDSTransfer.Create(nil);
  try
    T.AutoExecute := True;
    Cursor := Default(TOBDTransferCursor);
    Cursor.TotalBytes := 8;
    Cursor.BytesSent := 4;
    Cursor.MaxChunkBytes := 4;
    Cursor.NextBSC := 1;
    Assert.WillRaise(
      procedure begin
        T.Resume(Cursor, TBytes.Create($00, $01, $02, $03));
      end,
      EOBDConfig);
  finally
    T.Free;
  end;
end;

procedure TUDSTransferTests.InitialStateIsIdle;
var
  T: TOBDUDSTransfer;
begin
  T := TOBDUDSTransfer.Create(nil);
  try
    Assert.AreEqual(Ord(tsIdle), Ord(T.State));
  finally
    T.Free;
  end;
end;

procedure TUDSTransferTests.RetryDefaults;
var
  T: TOBDUDSTransfer;
begin
  T := TOBDUDSTransfer.Create(nil);
  try
    Assert.AreEqual(10, T.MaxPendingRetries);
    Assert.AreEqual(3, T.MaxChunkRetries);
    Assert.AreEqual(Cardinal(50), T.PendingDelayMs);
    Assert.AreEqual(Cardinal(20), T.ChunkRetryDelayMs);
  finally
    T.Free;
  end;
end;

{ ---- TOBDJ1939MemoryAccess ------------------------------------------------- }

procedure TJ1939MemoryAccessTests.DM14RoundTrip;
var
  Req, Decoded: TOBDJ1939DM14Request;
  Bytes: TBytes;
begin
  Req := Default(TOBDJ1939DM14Request);
  Req.Length_ := $1234;
  Req.Command := DM14_CMD_WRITE;
  Req.Pointer_ := $DEADBEEF;
  Req.PointerExtension := $05;
  Bytes := TOBDJ1939MemoryAccess.EncodeDM14(Req);
  Assert.AreEqual(8, Length(Bytes));
  Assert.IsTrue(TOBDJ1939MemoryAccess.DecodeDM14(Bytes, Decoded));
  Assert.AreEqual($1234, Integer(Decoded.Length_));
  Assert.AreEqual(Integer(DM14_CMD_WRITE), Integer(Decoded.Command));
  Assert.AreEqual($DEADBEEF, Integer(Decoded.Pointer_));
  Assert.AreEqual($05, Integer(Decoded.PointerExtension));
end;

procedure TJ1939MemoryAccessTests.DM14LengthOverflowRaises;
var
  Req: TOBDJ1939DM14Request;
begin
  Req := Default(TOBDJ1939DM14Request);
  Req.Length_ := $8000;
  Assert.WillRaise(
    procedure begin TOBDJ1939MemoryAccess.EncodeDM14(Req); end,
    EOBDConfig);
end;

procedure TJ1939MemoryAccessTests.DM15RoundTrip;
var
  Resp, Decoded: TOBDJ1939DM15Response;
  Bytes: TBytes;
begin
  Resp := Default(TOBDJ1939DM15Response);
  Resp.Length_ := $0010;
  Resp.Status := DM15_STATUS_PROCEED;
  Resp.EDCCounter := $42;
  Resp.EDCParameter := $1234;
  Resp.Seed := $ABCD;
  Bytes := TOBDJ1939MemoryAccess.EncodeDM15(Resp);
  Assert.AreEqual(8, Length(Bytes));
  Assert.IsTrue(TOBDJ1939MemoryAccess.DecodeDM15(Bytes, Decoded));
  Assert.AreEqual($0010, Integer(Decoded.Length_));
  Assert.AreEqual(Integer(DM15_STATUS_PROCEED), Integer(Decoded.Status));
  Assert.AreEqual($42,   Integer(Decoded.EDCCounter));
  Assert.AreEqual($1234, Integer(Decoded.EDCParameter));
  Assert.AreEqual($ABCD, Integer(Decoded.Seed));
end;

procedure TJ1939MemoryAccessTests.DM16RoundTrip;
var
  Data, Encoded, Decoded: TBytes;
begin
  Data := TBytes.Create($AA, $BB, $CC, $DD);
  Encoded := TOBDJ1939MemoryAccess.EncodeDM16(Data);
  Assert.AreEqual(5, Length(Encoded));
  Assert.AreEqual(4, Integer(Encoded[0]));
  Decoded := TOBDJ1939MemoryAccess.DecodeDM16(Encoded);
  Assert.AreEqual(4, Length(Decoded));
  Assert.AreEqual($CC, Integer(Decoded[2]));
end;

procedure TJ1939MemoryAccessTests.DM14CommandConstantsExposed;
begin
  Assert.AreEqual($00, Integer(DM14_CMD_ERASE));
  Assert.AreEqual($01, Integer(DM14_CMD_READ));
  Assert.AreEqual($02, Integer(DM14_CMD_WRITE));
  Assert.AreEqual($03, Integer(DM14_CMD_STATUS));
  Assert.AreEqual($06, Integer(DM14_CMD_BOOT_LOAD));
end;

initialization
  TDUnitX.RegisterTestFixture(TUDSTransferTests);
  TDUnitX.RegisterTestFixture(TJ1939MemoryAccessTests);

end.
