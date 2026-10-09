// ------------------------------------------------------------------------------
// ERD.Service.EVBattery.Request
//
// Portable construction and response validation for EV battery reads.
//
// Author      : Ernst Reidinga (ERDesigns)
// Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
// License     : MIT — see LICENSE
//
// History     :
// 2026-10-08  ERD  Route field reads and validate/strip identifier echoes.
// ------------------------------------------------------------------------------
unit ERD.Service.EVBattery.Request;

{$IFDEF FPC}
{$MODE DELPHI}
{$IF FPC_FULLVERSION >= 30301}
{$MODESWITCH FUNCTIONREFERENCES}
{$MODESWITCH ANONYMOUSFUNCTIONS}
{$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF}, ERD.Types, ERD.CAN.Route,
  ERD.Protocol.Types,
  ERD.Service.EVBattery.Types;

/// <summary>Construct a routed EV diagnostic request from a resolved rule.</summary>
/// <param name="ARule">Rule with inherited or overridden ECU routing.</param>
/// <returns>Request with service, identifier, CAN IDs and extended addressing.</returns>
/// <exception cref="EOBDConfig">Unsupported service or invalid CAN ID.</exception>
function MakeEVBatteryRequest(const ARule: TOBDEVBatteryRule): TOBDRequest;

/// <summary>Validate a positive reply and remove its DID/PID echo.</summary>
/// <param name="ARule">Rule used to issue the request.</param>
/// <param name="AResponse">Decoded protocol reply, with SID already removed.</param>
/// <returns>Payload bytes following the echoed identifier.</returns>
/// <exception cref="EOBDProtocolErr">Wrong SID, NRC, truncated or mismatched echo.</exception>
function EVBatteryResponseData(const ARule: TOBDEVBatteryRule;
  const AResponse: TOBDResponse): TBytes;

/// <summary>Select a catalog rule for an explicit vehicle model year.</summary>
/// <param name="ARule">Rule with an optional inclusive year range.</param>
/// <param name="AModelYear">Vehicle year; zero means unspecified.</param>
/// <returns>True for unrestricted rules or a matching year.</returns>
/// <exception cref="EOBDConfig">A model-dependent rule needs an explicit year.</exception>
function EVBatteryRuleApplies(const ARule: TOBDEVBatteryRule;
  AModelYear: Integer): Boolean;

implementation

function EVBatteryRuleApplies(const ARule: TOBDEVBatteryRule;
  AModelYear: Integer): Boolean;
begin
  if ((ARule.MinModelYear > 0) or (ARule.MaxModelYear > 0)) and (AModelYear = 0)
  then
    raise EOBDConfig.Create
      ('Set ModelYear before reading model-dependent EV rules');
  Result := ((ARule.MinModelYear = 0) or (AModelYear >= ARule.MinModelYear)) and
    ((ARule.MaxModelYear = 0) or (AModelYear <= ARule.MaxModelYear));
end;

function MakeEVBatteryRequest(const ARule: TOBDEVBatteryRule): TOBDRequest;
begin
  Result := MakeOBDRequest;
  Result.ServiceID := ARule.Service;
  case ARule.Service of
    $22:
      begin
        Result.Protocol := apUDS;
        Result.Data := TBytes.Create(Hi(ARule.DIDOrPID), Lo(ARule.DIDOrPID));
      end;
    $21, $01:
      begin
        if ARule.DIDOrPID > $FF then
          raise EOBDConfig.Create('Mode 01/21 PID exceeds one byte');
        if ARule.Service = $21 then
          Result.Protocol := apKWP2000;
        Result.Data := TBytes.Create(Lo(ARule.DIDOrPID));
      end;
  else
    raise EOBDConfig.Create('Unsupported EV battery read service');
  end;
  if ARule.RequestId <> 0 then
    Result.HeaderOverride := CANHeader(ARule.RequestId);
  if ARule.ResponseId <> 0 then
    Result.ResponseHeaderOverride := CANHeader(ARule.ResponseId);
  Result.UseExtendedAddressing := ARule.UseExtendedAddressing;
  Result.ExtendedTarget := ARule.ExtendedTarget;
  Result.ExtendedTester := ARule.ExtendedTester;
end;

function EVBatteryResponseData(const ARule: TOBDEVBatteryRule;
  const AResponse: TOBDResponse): TBytes;
var
  Echo: TBytes;
  I: Integer;
begin
  if AResponse.IsNegative then
    raise EOBDProtocolErr.CreateFmt('EV battery NRC 0x%.2X: %s',
      [AResponse.NRC, AResponse.NRCText]);
  if AResponse.ServiceID <> ARule.Service + $40 then
    raise EOBDProtocolErr.Create('EV battery response service mismatch');
  Echo := MakeEVBatteryRequest(ARule).Data;
  if Length(AResponse.Data) < Length(Echo) then
    raise EOBDProtocolErr.Create
      ('EV battery response identifier echo is truncated');
  for I := 0 to High(Echo) do
    if AResponse.Data[I] <> Echo[I] then
      raise EOBDProtocolErr.Create
        ('EV battery response identifier echo mismatch');
  Result := Copy(AResponse.Data, Length(Echo), Length(AResponse.Data) -
    Length(Echo));
end;

end.
