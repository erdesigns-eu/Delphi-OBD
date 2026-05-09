//------------------------------------------------------------------------------
//  Tests.OBD.Connection.Async
//
//  DUnitX coverage for TOBDConnection.OpenAsync. Exercises the
//  non-blocking open path against the Wi-Fi transport pointing at a
//  refused port — the open fails fast (network error), which lets us
//  verify (a) the call returns immediately, (b) OnError fires on the
//  main thread, (c) repeated calls while one is in flight raise.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 follow-up: OpenAsync.
//------------------------------------------------------------------------------

unit Tests.OBD.Connection.Async;

interface

uses
  DUnitX.TestFramework;

type
  /// <summary>
  ///   Coverage for <c>TOBDConnection.OpenAsync</c>.
  /// </summary>
  /// <remarks>
  ///   Tests use the Wi-Fi transport pointed at <c>127.0.0.1:1</c>
  ///   (always refused). This makes the open fail quickly and
  ///   deterministically, exercising the error path of the async
  ///   machinery without hardware.
  /// </remarks>
  [TestFixture]
  TConnectionAsyncTests = class
  public
    /// <summary>OpenAsync returns immediately and does not block on
    /// the connect attempt.</summary>
    [Test] procedure OpenAsyncReturnsImmediately;
    /// <summary>OnError fires from the main thread when the
    /// background open fails.</summary>
    [Test] procedure OnErrorFiresFromMainThread;
    /// <summary>Calling OpenAsync while an open is in flight raises
    /// EOBDConfig.</summary>
    [Test] procedure SecondOpenAsyncWhileInFlightRaises;
    /// <summary>Close cancels an in-flight async open cleanly.</summary>
    [Test] procedure CloseCancelsInFlightAsyncOpen;
    /// <summary>Free of a connection with an in-flight async open is
    /// clean (no leaks, no hang).</summary>
    [Test] procedure FreeWithInFlightAsyncOpenIsClean;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Settings,
  OBD.Connection.Retry,
  OBD.Connection;

const
  // Port 1 is the refused-port convention used across the suite.
  REFUSED_PORT = 1;

procedure ConfigureForRefusedConnect(C: TOBDConnection);
begin
  C.Transport := otWiFi;
  C.WiFiSettings.Host := '127.0.0.1';
  C.WiFiSettings.Port := REFUSED_PORT;
  C.WiFiSettings.ConnectTimeout := 1000;
  // Disable retry so the failure surfaces fast.
  C.RetryPolicy.Enabled := False;
  C.RetryPolicy.MaxAttempts := 1;
end;

function PumpUntilSignaled(AEvent: TEvent; ATimeoutMs: Cardinal): Boolean;
var
  Sw: TStopwatch;
begin
  // We're on the main thread. Drain queued events while waiting so
  // TThread.Queue dispatches fire.
  Sw := TStopwatch.StartNew;
  while Sw.ElapsedMilliseconds < ATimeoutMs do
  begin
    CheckSynchronize(50);
    if AEvent.WaitFor(50) = wrSignaled then
      Exit(True);
  end;
  Result := False;
end;

procedure TConnectionAsyncTests.OpenAsyncReturnsImmediately;
var
  C: TOBDConnection;
  Sw: TStopwatch;
begin
  C := TOBDConnection.Create(nil);
  try
    ConfigureForRefusedConnect(C);
    Sw := TStopwatch.StartNew;
    C.OpenAsync;
    // Returning quickly is the contract; even a refused TCP connect
    // takes a few ms. 100 ms is loose but unambiguous.
    Assert.IsTrue(Sw.ElapsedMilliseconds < 100,
      Format('OpenAsync took %d ms (should return immediately)',
        [Sw.ElapsedMilliseconds]));
    // Wait for the worker to finish so destructor cleanup is clean.
    C.Close;
  finally
    C.Free;
  end;
end;

procedure TConnectionAsyncTests.OnErrorFiresFromMainThread;
var
  C: TOBDConnection;
  ErrorEvent: TEvent;
  ErrorThreadID: TThreadID;
  ErrorMsg: string;
begin
  C := TOBDConnection.Create(nil);
  ErrorEvent := TEvent.Create(nil, True, False, '');
  try
    ConfigureForRefusedConnect(C);
    C.OnError :=
      procedure(Sender: TObject; ACode: TOBDErrorCode;
        const AMessage: string; var AHandled: Boolean)
      begin
        ErrorThreadID := TThread.CurrentThread.ThreadID;
        ErrorMsg := AMessage;
        ErrorEvent.SetEvent;
      end;
    C.OpenAsync;
    Assert.IsTrue(PumpUntilSignaled(ErrorEvent, 5000),
      'OnError did not fire within 5 s');
    Assert.AreEqual(MainThreadID, ErrorThreadID,
      'OnError must fire from the main thread');
    Assert.IsNotEmpty(ErrorMsg);
  finally
    ErrorEvent.Free;
    C.Free;
  end;
end;

procedure TConnectionAsyncTests.SecondOpenAsyncWhileInFlightRaises;
var
  C: TOBDConnection;
begin
  C := TOBDConnection.Create(nil);
  try
    // Use a host that will hang during DNS / connect for long enough
    // that the second call sees the in-flight worker. 192.0.2.x is
    // reserved (RFC 5737) and unroutable on most hosts → hangs the
    // connect attempt.
    C.Transport := otWiFi;
    C.WiFiSettings.Host := '192.0.2.1';
    C.WiFiSettings.Port := REFUSED_PORT;
    C.WiFiSettings.ConnectTimeout := 5000;
    C.RetryPolicy.Enabled := False;

    C.OpenAsync;
    // Race-y if the first call already finished; mitigate by sleeping
    // a bit to let the worker spin up.
    TThread.Sleep(50);
    Assert.WillRaise(
      procedure
      begin
        C.OpenAsync;
      end,
      EOBDConfig);
  finally
    C.Close; // cancels the in-flight open
    C.Free;
  end;
end;

procedure TConnectionAsyncTests.CloseCancelsInFlightAsyncOpen;
var
  C: TOBDConnection;
  Sw: TStopwatch;
begin
  C := TOBDConnection.Create(nil);
  try
    C.Transport := otWiFi;
    C.WiFiSettings.Host := '192.0.2.1';
    C.WiFiSettings.Port := REFUSED_PORT;
    C.WiFiSettings.ConnectTimeout := 30000; // worker would hang for 30 s
    C.RetryPolicy.Enabled := False;
    C.OpenAsync;
    TThread.Sleep(50);
    Sw := TStopwatch.StartNew;
    C.Close;
    // Close should cancel and join in well under 30 s.
    Assert.IsTrue(Sw.ElapsedMilliseconds < 10000,
      Format('Close blocked for %d ms (cancel did not propagate)',
        [Sw.ElapsedMilliseconds]));
  finally
    C.Free;
  end;
end;

procedure TConnectionAsyncTests.FreeWithInFlightAsyncOpenIsClean;
var
  C: TOBDConnection;
begin
  C := TOBDConnection.Create(nil);
  try
    C.Transport := otWiFi;
    C.WiFiSettings.Host := '192.0.2.1';
    C.WiFiSettings.Port := REFUSED_PORT;
    C.WiFiSettings.ConnectTimeout := 30000;
    C.RetryPolicy.Enabled := False;
    C.OpenAsync;
    TThread.Sleep(50);
  finally
    C.Free; // Destructor must cancel + join the worker.
  end;
  Assert.Pass;
end;

initialization
  TDUnitX.RegisterTestFixture(TConnectionAsyncTests);

end.
