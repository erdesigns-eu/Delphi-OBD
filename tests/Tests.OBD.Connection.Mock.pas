//------------------------------------------------------------------------------
//  Tests.OBD.Connection.Mock
//
//  DUnitX coverage for TOBDMockTransport. Verifies the mock honours
//  the IOBDConnectionTransport contract: state transitions, write
//  capture, feed dispatch, and the under-test enforcement of "no I/O
//  while closed".
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//------------------------------------------------------------------------------

unit Tests.OBD.Connection.Mock;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>
  ///   Coverage for <c>TOBDMockTransport</c>.
  /// </summary>
  [TestFixture]
  TConnectionMockTests = class
  public
    /// <summary>Newly-created mock starts in <c>csClosed</c>.</summary>
    [Test] procedure StartsClosed;
    /// <summary>SimulateOpen transitions through opening to open.</summary>
    [Test] procedure SimulateOpenTransitionsState;
    /// <summary>WriteBytes captures bytes when open.</summary>
    [Test] procedure WriteWhileOpenCaptures;
    /// <summary>WriteBytes raises EOBDNotConnected when closed.</summary>
    [Test] procedure WriteWhileClosedRaises;
    /// <summary>FeedBytes invokes the data-received handler.</summary>
    [Test] procedure FeedDispatchesToHandler;
    /// <summary>SimulateError invokes the error handler with code +
    /// message.</summary>
    [Test] procedure SimulateErrorDispatches;
    /// <summary>WrittenString decodes captured bytes as ASCII.</summary>
    [Test] procedure WrittenStringIsASCIIDecode;
    /// <summary>ClearWritten resets the capture.</summary>
    [Test] procedure ClearWrittenResetsCapture;
    /// <summary>State transitions fire OnStateChanged.</summary>
    [Test] procedure StateChangedFiresOnTransitions;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Mock;

procedure TConnectionMockTests.StartsClosed;
var
  Mock: TOBDMockTransport;
begin
  Mock := TOBDMockTransport.Create;
  try
    Assert.AreEqual(Ord(csClosed), Ord(Mock.State));
    Assert.IsFalse(Mock.IsOpen);
  finally
    Mock.Free;
  end;
end;

procedure TConnectionMockTests.SimulateOpenTransitionsState;
var
  Mock: TOBDMockTransport;
begin
  Mock := TOBDMockTransport.Create;
  try
    Mock.SimulateOpen;
    Assert.AreEqual(Ord(csOpen), Ord(Mock.State));
    Assert.IsTrue(Mock.IsOpen);
    Mock.Close;
    Assert.AreEqual(Ord(csClosed), Ord(Mock.State));
  finally
    Mock.Free;
  end;
end;

procedure TConnectionMockTests.WriteWhileOpenCaptures;
var
  Mock: TOBDMockTransport;
  Got: TBytes;
begin
  Mock := TOBDMockTransport.Create;
  try
    Mock.SimulateOpen;
    Mock.WriteBytes(TBytes.Create($41, $54, $5A));
    Got := Mock.Written;
    Assert.AreEqual<NativeInt>(3, Length(Got));
    Assert.AreEqual<Byte>($41, Got[0]);
    Assert.AreEqual<Byte>($54, Got[1]);
    Assert.AreEqual<Byte>($5A, Got[2]);
  finally
    Mock.Free;
  end;
end;

procedure TConnectionMockTests.WriteWhileClosedRaises;
var
  Mock: TOBDMockTransport;
begin
  Mock := TOBDMockTransport.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        Mock.WriteBytes(TBytes.Create(1));
      end,
      EOBDNotConnected);
  finally
    Mock.Free;
  end;
end;

procedure TConnectionMockTests.FeedDispatchesToHandler;
var
  Mock: TOBDMockTransport;
  Received: TBytes;
begin
  Mock := TOBDMockTransport.Create;
  try
    Mock.OnDataReceived :=
      procedure(Sender: TObject; const ABytes: TBytes)
      begin
        Received := Copy(ABytes);
      end;
    Mock.SimulateOpen;
    Mock.FeedString('OK>');
    Assert.AreEqual<NativeInt>(3, Length(Received));
    Assert.AreEqual('OK>', TEncoding.ASCII.GetString(Received));
  finally
    Mock.Free;
  end;
end;

procedure TConnectionMockTests.SimulateErrorDispatches;
var
  Mock: TOBDMockTransport;
  CapturedCode: TOBDErrorCode;
  CapturedMsg: string;
begin
  Mock := TOBDMockTransport.Create;
  CapturedCode := oeNone;
  CapturedMsg := '';
  try
    Mock.OnTransportError :=
      procedure(Sender: TObject; ACode: TOBDErrorCode; const AMessage: string)
      begin
        CapturedCode := ACode;
        CapturedMsg := AMessage;
      end;
    Mock.SimulateError(oeTimeout, 'timed out');
    Assert.AreEqual(Ord(oeTimeout), Ord(CapturedCode));
    Assert.AreEqual('timed out', CapturedMsg);
  finally
    Mock.Free;
  end;
end;

procedure TConnectionMockTests.WrittenStringIsASCIIDecode;
var
  Mock: TOBDMockTransport;
begin
  Mock := TOBDMockTransport.Create;
  try
    Mock.SimulateOpen;
    Mock.WriteBytes(TEncoding.ASCII.GetBytes('ATZ' + #13));
    Assert.AreEqual('ATZ' + #13, Mock.WrittenString);
  finally
    Mock.Free;
  end;
end;

procedure TConnectionMockTests.ClearWrittenResetsCapture;
var
  Mock: TOBDMockTransport;
begin
  Mock := TOBDMockTransport.Create;
  try
    Mock.SimulateOpen;
    Mock.WriteBytes(TBytes.Create(1, 2, 3));
    Assert.AreEqual<NativeInt>(3, Length(Mock.Written));
    Mock.ClearWritten;
    Assert.AreEqual<NativeInt>(0, Length(Mock.Written));
  finally
    Mock.Free;
  end;
end;

procedure TConnectionMockTests.StateChangedFiresOnTransitions;
var
  Mock: TOBDMockTransport;
  States: TArray<TOBDConnectionState>;
begin
  Mock := TOBDMockTransport.Create;
  try
    Mock.OnStateChanged :=
      procedure(Sender: TObject; NewState: TOBDConnectionState)
      begin
        SetLength(States, Length(States) + 1);
        States[High(States)] := NewState;
      end;
    Mock.SimulateOpen;
    Mock.Close;
    // Expect csOpening, csOpen, csClosing, csClosed.
    Assert.AreEqual<NativeInt>(4, Length(States));
    Assert.AreEqual(Ord(csOpening), Ord(States[0]));
    Assert.AreEqual(Ord(csOpen),    Ord(States[1]));
    Assert.AreEqual(Ord(csClosing), Ord(States[2]));
    Assert.AreEqual(Ord(csClosed),  Ord(States[3]));
  finally
    Mock.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TConnectionMockTests);

end.
