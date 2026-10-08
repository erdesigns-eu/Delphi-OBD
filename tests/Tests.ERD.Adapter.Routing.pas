//------------------------------------------------------------------------------
//  Tests.ERD.Adapter.Routing
//
//  Routed exchanges execute the production adapter lock/command loop against
//  an overridden wire exchange, without ECU or transport hardware.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-10-08  ERD  Routing plans, rejection and concurrent serialization.
//------------------------------------------------------------------------------
unit Tests.ERD.Adapter.Routing;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF}, {$IFDEF FPC}Classes{$ELSE}System.Classes{$ENDIF}, DUnitX.TestFramework,
  ERD.Types, ERD.Adapter, ERD.Adapter.Types;

type
  TRecordingAdapter = class(TOBDAdapter)
  strict private
    FCommands: TStringList;
    FRejectedCommand: string;
  protected
    function ExecuteCommand(const ACommand: string;
      ATimeoutMs: Cardinal): TOBDAdapterResponse; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Commands: TStringList read FCommands;
    property RejectedCommand: string read FRejectedCommand write FRejectedCommand;
  end;

  [TestFixture]
  TAdapterRoutingTests = class
  public
    [Test] procedure ExtendedRoutingPrecedesDiagnosticRequest;
    [Test] procedure RejectedRouteDoesNotSendDiagnosticRequest;
    [Test] procedure InvalidRouteDoesNotSendAnyCommand;
    [Test] procedure ConcurrentRoutesDoNotInterleave;
  end;

implementation

constructor TRecordingAdapter.Create(AOwner: TComponent);
begin
  inherited;
  FCommands := TStringList.Create;
end;

destructor TRecordingAdapter.Destroy;
begin
  FCommands.Free;
  inherited;
end;

function TRecordingAdapter.ExecuteCommand(const ACommand: string;
  ATimeoutMs: Cardinal): TOBDAdapterResponse;
begin
  FCommands.Add(ACommand);
  TThread.Sleep(1);
  Result := Default(TOBDAdapterResponse);
  Result.Command := ACommand;
  Result.Raw := 'OK';
  if ACommand = FRejectedCommand then
  begin
    Result.IsError := True;
    Result.Raw := '?';
  end;
end;

procedure TAdapterRoutingTests.ExtendedRoutingPrecedesDiagnosticRequest;
var
  Adapter: TRecordingAdapter;
begin
  Adapter := TRecordingAdapter.Create(nil);
  try
    Adapter.WriteRoutedOBDCommand('22DDB7', '6F1', '607', True, $07, $F1);
    Assert.AreEqual(5, Adapter.Commands.Count);
    Assert.AreEqual('ATSH6F1', Adapter.Commands[0]);
    Assert.AreEqual('ATCRA607', Adapter.Commands[1]);
    Assert.AreEqual('ATCEA07', Adapter.Commands[2]);
    Assert.AreEqual('ATCERF1', Adapter.Commands[3]);
    Assert.AreEqual('22DDB7', Adapter.Commands[4]);
  finally
    Adapter.Free;
  end;
end;

procedure TAdapterRoutingTests.RejectedRouteDoesNotSendDiagnosticRequest;
var
  Adapter: TRecordingAdapter;
begin
  Adapter := TRecordingAdapter.Create(nil);
  try
    Adapter.RejectedCommand := 'ATCRA607';
    Assert.WillRaise(
      procedure
      begin
        Adapter.WriteRoutedOBDCommand('22DDB7', '6F1', '607', True, $07, $F1);
      end, EOBDAdapter);
    Assert.AreEqual(2, Adapter.Commands.Count);
    Assert.AreEqual(-1, Adapter.Commands.IndexOf('22DDB7'));
  finally
    Adapter.Free;
  end;
end;

procedure TAdapterRoutingTests.InvalidRouteDoesNotSendAnyCommand;
var
  Adapter: TRecordingAdapter;
begin
  Adapter := TRecordingAdapter.Create(nil);
  try
    Assert.WillRaise(
      procedure
      begin
        Adapter.WriteRoutedOBDCommand('22DDB7', '800', '', False, 0, 0);
      end, EOBDConfig);
    Assert.AreEqual(0, Adapter.Commands.Count);
  finally
    Adapter.Free;
  end;
end;

procedure TAdapterRoutingTests.ConcurrentRoutesDoNotInterleave;
var
  Adapter: TRecordingAdapter;
  First, Second: TThread;
  Offset: Integer;
begin
  Adapter := TRecordingAdapter.Create(nil);
  First := nil;
  Second := nil;
  try
    First := TThread.CreateAnonymousThread(
      procedure
      begin
        Adapter.WriteRoutedOBDCommand('221234', '7E4', '7EC', False, 0, 0);
      end);
    Second := TThread.CreateAnonymousThread(
      procedure
      begin
        Adapter.WriteRoutedOBDCommand('225678', '7E0', '7E8', False, 0, 0);
      end);
    First.FreeOnTerminate := False;
    Second.FreeOnTerminate := False;
    First.Start;
    Second.Start;
    First.WaitFor;
    Second.WaitFor;
    Assert.IsNull(First.FatalException);
    Assert.IsNull(Second.FatalException);
    Assert.AreEqual(8, Adapter.Commands.Count);
    for Offset in [0, 4] do
    begin
      Assert.AreEqual('ATCEA', Adapter.Commands[Offset + 2]);
      if Adapter.Commands[Offset] = 'ATSH7E4' then
      begin
        Assert.AreEqual('ATCRA7EC', Adapter.Commands[Offset + 1]);
        Assert.AreEqual('221234', Adapter.Commands[Offset + 3]);
      end
      else
      begin
        Assert.AreEqual('ATSH7E0', Adapter.Commands[Offset]);
        Assert.AreEqual('ATCRA7E8', Adapter.Commands[Offset + 1]);
        Assert.AreEqual('225678', Adapter.Commands[Offset + 3]);
      end;
    end;
  finally
    First.Free;
    Second.Free;
    Adapter.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TAdapterRoutingTests);

end.
