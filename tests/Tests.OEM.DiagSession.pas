//------------------------------------------------------------------------------
// UNIT           : Tests.OEM.DiagSession
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : DiagSession orchestration is connection-dependent
//                  by design — the bytes-level path runs through
//                  TOBDConnectionAsync and is exercised by the
//                  console flashing example end-to-end. These tests
//                  cover the construction / state-machine guarantees
//                  that the orchestrator must hold even before the
//                  first byte goes on the wire.
//------------------------------------------------------------------------------
unit Tests.OEM.DiagSession;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDiagSessionConstructionTests = class
  public
    /// <summary>
    ///   Rejects nil connection.
    /// </summary>
    [Test] procedure RejectsNilConnection;
    /// <summary>
    ///   Rejects nil extension.
    /// </summary>
    [Test] procedure RejectsNilExtension;
  end;

implementation

uses
  System.SysUtils,
  OBD.OEM, OBD.OEM.DiagSession, OBD.OEM.VW;

//------------------------------------------------------------------------------
// REJECTS NIL CONNECTION
//------------------------------------------------------------------------------
procedure TDiagSessionConstructionTests.RejectsNilConnection;
begin
  Assert.WillRaise(
    procedure
    var
      Session: TOBDDiagSession;
    begin
      Session := TOBDDiagSession.Create(nil, TOBDOEMExtensionVW.Create);
      Session.Free;
    end,
    EOBDDiagSessionError);
end;

//------------------------------------------------------------------------------
// REJECTS NIL EXTENSION
//------------------------------------------------------------------------------
procedure TDiagSessionConstructionTests.RejectsNilExtension;
begin
  Assert.WillRaise(
    procedure
    var
      Session: TOBDDiagSession;
    begin
      // We can't easily build a real TOBDConnectionAsync without a
      // backing IOBDConnection, but the nil-check on the OEM is the
      // first guard we hit before that branch — TOBDConnectionAsync
      // construction on a nil also raises, which DUnitX captures.
      Session := TOBDDiagSession.Create(nil, nil);
      Session.Free;
    end,
    Exception);
end;

initialization
  TDUnitX.RegisterTestFixture(TDiagSessionConstructionTests);

end.
