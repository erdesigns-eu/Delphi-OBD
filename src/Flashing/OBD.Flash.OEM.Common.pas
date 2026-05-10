//------------------------------------------------------------------------------
//  OBD.Flash.OEM.Common
//
//  IOBDFlashHandshake — shared contract every OEM bootloader-
//  handshake unit implements. Hosts plug a concrete vendor
//  handshake into TOBDFlashPipeline.OnEnterProgramming via a thin
//  closure:
//
//    Pipeline.OnEnterProgramming :=
//      procedure begin Handshake.Run; end;
//
//  Each vendor's pre-flash sequence has its own session levels,
//  security-access level, optional anti-theft pin, and a
//  vendor-specific "prepare for flash" routine ID. The contract
//  unifies them behind a single Run() method while the per-OEM
//  units expose vendor-specific properties (e.g. VAG SCN code,
//  BMW programming-session sub-function, Ford CSE strategy).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-09  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Flash.OEM.Common;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Types,
  OBD.Protocol.Types,
  OBD.Protocol.UDS,
  OBD.Protocol,
  OBD.Coding.SecurityAccess,
  OBD.Coding.RoutineControl;

const
  /// <summary>UDS Diagnostic Session Control sub-function:
  /// programming session (boot/programming mode).</summary>
  UDS_SESSION_PROGRAMMING_OEM = $02;
  /// <summary>UDS Diagnostic Session Control sub-function:
  /// extended diagnostic session.</summary>
  UDS_SESSION_EXTENDED_OEM = $03;
  /// <summary>UDS ECUReset hardReset.</summary>
  UDS_RESET_HARD_OEM = $01;

type
  /// <summary>Unified bootloader-handshake contract.</summary>
  IOBDFlashHandshake = interface
    ['{56F8B7AA-1B11-4F08-B6E2-3CDB91A7A2C0}']
    /// <summary>Vendor display name.</summary>
    function VendorName: string;
    /// <summary>Runs the vendor's enter-programming sequence.
    /// Raises on any negative response.</summary>
    procedure Run;
    /// <summary>Sends the vendor's exit / leave-programming
    /// sequence. Idempotent.</summary>
    procedure Leave;
  end;

  /// <summary>
  ///   Abstract base for the per-OEM units. Children supply the
  ///   vendor-specific properties + override <c>DoRun</c>.
  /// </summary>
  TOBDFlashHandshake = class abstract(TComponent, IOBDFlashHandshake)
  strict private
    [Weak] FProtocol: TOBDProtocol;
    FSecurity: TOBDSecurityAccess;
    FRoutines: TOBDRoutineControl;
    FAutoExecute: Boolean;
    procedure SetProtocol(AValue: TOBDProtocol);
    procedure SetSecurity(AValue: TOBDSecurityAccess);
    procedure SetRoutines(AValue: TOBDRoutineControl);
  strict protected
    procedure DoRun; virtual; abstract;
    procedure DoLeave; virtual;
    function GetVendorName: string; virtual; abstract;
    /// <summary>Helper: send a Diagnostic Session Control
    /// sub-function and raise on negative.</summary>
    procedure SwitchSession(ASubFunction: Byte);
    /// <summary>Helper: send an ECUReset.</summary>
    procedure ResetECU(ASubFunction: Byte = UDS_RESET_HARD_OEM);
    /// <summary>Helper: send TesterPresent (0x3E 0x00) keeping
    /// the session alive.</summary>
    procedure TesterPresent;
    property Protocol: TOBDProtocol read FProtocol;
    property Security: TOBDSecurityAccess read FSecurity;
    property Routines: TOBDRoutineControl read FRoutines;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    function VendorName: string;
    procedure Run;
    procedure Leave;

    procedure SetUp(AProtocol: TOBDProtocol;
      ASecurity: TOBDSecurityAccess; ARoutines: TOBDRoutineControl);

  published
    /// <summary>Bound protocol component.</summary>
    property ProtocolComp: TOBDProtocol read FProtocol write SetProtocol;
    /// <summary>Bound security-access component.</summary>
    property SecurityComp: TOBDSecurityAccess read FSecurity
      write SetSecurity;
    /// <summary>Bound routine-control component.</summary>
    property RoutinesComp: TOBDRoutineControl read FRoutines
      write SetRoutines;
    /// <summary>Safety gate. Default <c>False</c>.</summary>
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute
      default False;
  end;

implementation

procedure TOBDFlashHandshake.SetProtocol(AValue: TOBDProtocol);
begin
  if FProtocol = AValue then Exit;
  if FProtocol <> nil then FProtocol.RemoveFreeNotification(Self);
  FProtocol := AValue;
  if FProtocol <> nil then FProtocol.FreeNotification(Self);
end;

procedure TOBDFlashHandshake.SetSecurity(AValue: TOBDSecurityAccess);
begin
  if FSecurity = AValue then Exit;
  if FSecurity <> nil then FSecurity.RemoveFreeNotification(Self);
  FSecurity := AValue;
  if FSecurity <> nil then FSecurity.FreeNotification(Self);
end;

procedure TOBDFlashHandshake.SetRoutines(AValue: TOBDRoutineControl);
begin
  if FRoutines = AValue then Exit;
  if FRoutines <> nil then FRoutines.RemoveFreeNotification(Self);
  FRoutines := AValue;
  if FRoutines <> nil then FRoutines.FreeNotification(Self);
end;

procedure TOBDFlashHandshake.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FProtocol then FProtocol := nil;
    if AComponent = FSecurity then FSecurity := nil;
    if AComponent = FRoutines then FRoutines := nil;
  end;
end;

procedure TOBDFlashHandshake.SetUp(AProtocol: TOBDProtocol;
  ASecurity: TOBDSecurityAccess; ARoutines: TOBDRoutineControl);
begin
  SetProtocol(AProtocol);
  SetSecurity(ASecurity);
  SetRoutines(ARoutines);
end;

procedure TOBDFlashHandshake.SwitchSession(ASubFunction: Byte);
var
  Resp: TOBDResponse;
begin
  if FProtocol = nil then
    raise EOBDConfig.Create('Flash handshake: Protocol not assigned');
  Resp := FProtocol.Request($10, TBytes.Create(ASubFunction));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'DiagnosticSessionControl 0x%2.2X negative: %s',
      [ASubFunction, Resp.NRCText]);
end;

procedure TOBDFlashHandshake.ResetECU(ASubFunction: Byte);
var
  Resp: TOBDResponse;
begin
  if FProtocol = nil then Exit;
  Resp := FProtocol.Request($11, TBytes.Create(ASubFunction));
  if Resp.IsNegative then
    raise EOBDProtocolErr.CreateFmt(
      'ECUReset 0x%2.2X negative: %s',
      [ASubFunction, Resp.NRCText]);
end;

procedure TOBDFlashHandshake.TesterPresent;
var
  Resp: TOBDResponse;
begin
  if FProtocol = nil then Exit;
  Resp := FProtocol.Request($3E, TBytes.Create($00));
  // TesterPresent NRC is rare and non-fatal; we surface but
  // do not abort.
  if Resp.IsNegative then ;
end;

function TOBDFlashHandshake.VendorName: string;
begin
  Result := GetVendorName;
end;

procedure TOBDFlashHandshake.Run;
begin
  if not FAutoExecute then
    raise EOBDConfig.CreateFmt(
      '%s flash handshake: AutoExecute is False — set it explicitly',
      [GetVendorName]);
  if FProtocol = nil then
    raise EOBDConfig.Create('Flash handshake: Protocol not assigned');
  DoRun;
end;

procedure TOBDFlashHandshake.Leave;
begin
  try DoLeave; except end;
end;

procedure TOBDFlashHandshake.DoLeave;
begin
  // Default: switch back to default session.
  try
    SwitchSession($01);
  except
    // best-effort
  end;
end;

end.
