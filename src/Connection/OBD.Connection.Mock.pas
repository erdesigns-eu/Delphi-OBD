//------------------------------------------------------------------------------
//  OBD.Connection.Mock
//
//  In-memory mock transport used by the test suite. Lets a test feed
//  scripted bytes "from the wire" and inspect bytes that were written
//  by the system under test, without any real I/O. Implements the same
//  IOBDConnectionTransport contract as the real transports so any code
//  that talks to a transport can be exercised against this.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Phase 2 initial.
//    2026-05-09  ERD  Phase 2 follow-up: rebased onto TOBDBaseTransport.
//------------------------------------------------------------------------------

unit OBD.Connection.Mock;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  OBD.Types,
  OBD.Connection.Types,
  OBD.Connection.Transport.Base;

type
  /// <summary>
  ///   Mock transport for tests. Feed bytes via <c>FeedBytes</c> /
  ///   <c>FeedString</c>, inspect captured writes via
  ///   <c>Written</c> / <c>WrittenString</c>.
  /// </summary>
  /// <remarks>
  ///   No internal worker thread; receive callbacks fire on whichever
  ///   thread called <c>FeedBytes</c>. Tests that exercise the main-
  ///   thread marshalling in <c>TOBDConnection</c> should call
  ///   <c>FeedBytes</c> from a worker thread.
  /// </remarks>
  TOBDMockTransport = class(TOBDBaseTransport)
  strict private
    FWritten: TBytes;
  public
    /// <summary>Closes the mock. Transitions
    /// <c>csClosing</c> -> <c>csClosed</c>.</summary>
    procedure Close; override;

    /// <summary>Captures bytes for later inspection.</summary>
    /// <param name="ABytes">Bytes to "send".</param>
    /// <returns>Always <c>Length(ABytes)</c> when open.</returns>
    /// <exception cref="EOBDNotConnected">Mock is not open.</exception>
    function WriteBytes(const ABytes: TBytes): Integer; override;

    /// <summary>Marks the mock as open
    /// (<c>csOpening</c> -> <c>csOpen</c>).</summary>
    procedure SimulateOpen;
    /// <summary>Fires <c>OnDataReceived</c> as if <c>ABytes</c>
    /// arrived from the wire.</summary>
    /// <param name="ABytes">Bytes to deliver.</param>
    procedure FeedBytes(const ABytes: TBytes);
    /// <summary>Convenience for <see cref="FeedBytes"/> with an ASCII
    /// string.</summary>
    /// <param name="AText">Text to deliver as ASCII bytes.</param>
    procedure FeedString(const AText: string);
    /// <summary>Fires <c>OnTransportError</c> with the supplied code
    /// and message.</summary>
    /// <param name="ACode">Coded error.</param>
    /// <param name="AMessage">Human-readable message.</param>
    procedure SimulateError(ACode: TOBDErrorCode; const AMessage: string);
    /// <summary>Fires <c>OnProgress</c> with a step-style snapshot —
    /// useful for tests that want to assert main-thread marshalling
    /// of progress events.</summary>
    /// <param name="AIndex">1-based step index.</param>
    /// <param name="ACount">Total expected steps.</param>
    /// <param name="AName">Phase name.</param>
    /// <param name="ADetail">Optional sub-detail.</param>
    procedure SimulateProgress(AIndex, ACount: Cardinal;
      const AName: string; const ADetail: string = '');

    /// <summary>Snapshot of bytes captured by <see cref="WriteBytes"/>
    /// since the last <see cref="ClearWritten"/>.</summary>
    /// <returns>Defensive copy.</returns>
    function Written: TBytes;
    /// <summary>Snapshot of captured bytes decoded as ASCII.</summary>
    /// <returns>ASCII string. Empty when nothing has been written.</returns>
    function WrittenString: string;
    /// <summary>Resets the capture buffer.</summary>
    procedure ClearWritten;
  end;

implementation

{ ---- TOBDMockTransport ------------------------------------------------------- }

procedure TOBDMockTransport.Close;
begin
  SetState(csClosing);
  SetState(csClosed);
end;

function TOBDMockTransport.WriteBytes(const ABytes: TBytes): Integer;
var
  OldLen: Integer;
begin
  FLock.Enter;
  try
    if FState <> csOpen then
      raise EOBDNotConnected.Create('Mock transport is not open');
    OldLen := Length(FWritten);
    SetLength(FWritten, OldLen + Length(ABytes));
    if Length(ABytes) > 0 then
      Move(ABytes[0], FWritten[OldLen], Length(ABytes));
    Result := Length(ABytes);
  finally
    FLock.Leave;
  end;
end;

procedure TOBDMockTransport.SimulateOpen;
begin
  SetState(csOpening);
  SetState(csOpen);
end;

procedure TOBDMockTransport.FeedBytes(const ABytes: TBytes);
begin
  FireBytes(ABytes);
end;

procedure TOBDMockTransport.FeedString(const AText: string);
begin
  FireBytes(TEncoding.ASCII.GetBytes(AText));
end;

procedure TOBDMockTransport.SimulateError(ACode: TOBDErrorCode;
  const AMessage: string);
begin
  FireError(ACode, AMessage);
end;

procedure TOBDMockTransport.SimulateProgress(AIndex, ACount: Cardinal;
  const AName: string; const ADetail: string);
begin
  FireProgress(AIndex, ACount, AName, ADetail);
end;

function TOBDMockTransport.Written: TBytes;
begin
  FLock.Enter;
  try
    Result := Copy(FWritten);
  finally
    FLock.Leave;
  end;
end;

function TOBDMockTransport.WrittenString: string;
var
  Bytes: TBytes;
begin
  Bytes := Written;
  if Length(Bytes) = 0 then
    Exit('');
  Result := TEncoding.ASCII.GetString(Bytes);
end;

procedure TOBDMockTransport.ClearWritten;
begin
  FLock.Enter;
  try
    SetLength(FWritten, 0);
  finally
    FLock.Leave;
  end;
end;

end.
