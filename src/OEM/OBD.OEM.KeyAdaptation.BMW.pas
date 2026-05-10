//------------------------------------------------------------------------------
//  OBD.OEM.KeyAdaptation.BMW
//
//  BMW EWS / CAS / FEM / BDC key adaptation. Modern BMWs
//  (F-series CAS4 / G-series FEM_BDC) require an ISTA / E-Sys
//  PIN (ISN) plus the on-bus security-access seed/key
//  exchange. The component drives the documented
//  E-Sys / Tool32 procedure: SecurityAccess on the FEM
//  module, then routine 0x4001 (add key) / 0x4002 (clear keys).
//
//  Pre-CAS3 platforms (E36 / E39 / E46 / E53 EWS-3 / EWS-4)
//  use the older EWS protocol that this component does NOT
//  drive over OBD - those need an EWS adapter / soldered
//  pinout. ListSlots will report "EWS legacy - not OBD-
//  addressable".
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.OEM.KeyAdaptation.BMW;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Protocol.Types,
  OBD.OEM.KeyAdaptation.Types,
  OBD.OEM.KeyAdaptation.Base;

type
  TOBDKeyAdaptationBMW = class(TOBDKeyAdaptationBase)
  protected
    function DoListSlots: TOBDKeyAdaptResult; override;
    function DoAddKey: TOBDKeyAdaptResult; override;
    function DoClearOneSlot(ASlotIndex: Byte): TOBDKeyAdaptResult; override;
    function DoClearAllKeys: TOBDKeyAdaptResult; override;
    function DoCheckPin: TOBDKeyAdaptResult; override;
  public
    function RequiresChassisCode: Boolean; override;
  end;

implementation

uses
  OBD.Protocol;

const
  ROUTINE_BMW_ADD_KEY    = $4001;
  ROUTINE_BMW_CLEAR_KEYS = $4002;
  // FEM/BDC slot inventory DID.
  DID_BMW_KEY_SLOTS      = $4400;

function TOBDKeyAdaptationBMW.RequiresChassisCode: Boolean;
begin
  Result := True;
end;

function TOBDKeyAdaptationBMW.DoListSlots: TOBDKeyAdaptResult;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
  I:    Integer;
begin
  Result := Default(TOBDKeyAdaptResult);
  // Reject EWS-era chassis codes upfront; the legacy EWS
  // protocol isn't OBD-addressable.
  if (LowerCase(ChassisCode) = 'e36') or
     (LowerCase(ChassisCode) = 'e39') or
     (LowerCase(ChassisCode) = 'e46') or
     (LowerCase(ChassisCode) = 'e53') then
  begin
    Result.Message :=
      'BMW EWS-era chassis (' + ChassisCode + ') is not ' +
      'OBD-addressable for key adaptation - needs EWS pinout';
    Exit;
  end;
  try
    Req := MakeOBDRequest;
    Req.ServiceID := $22;
    Req.Data := TBytes.Create(Hi(DID_BMW_KEY_SLOTS),
                              Lo(DID_BMW_KEY_SLOTS));
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('FEM/BDC NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    if Length(Resp.Data) < 1 then
    begin
      Result.Message := 'Truncated FEM/BDC slot response';
      Exit;
    end;
    // FEM/BDC supports up to 10 keys; one byte per slot,
    // 0x00 = empty, otherwise the FBS-key byte.
    SetLength(Result.Slots, Length(Resp.Data));
    for I := 0 to High(Resp.Data) do
    begin
      Result.Slots[I].Index  := I + 1;
      Result.Slots[I].Filled := Resp.Data[I] <> $00;
      if Result.Slots[I].Filled then
        Result.Slots[I].Label_ := Format('FBS=0x%.2X', [Resp.Data[I]]);
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationBMW.DoAddKey: TOBDKeyAdaptResult;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
begin
  Result := Default(TOBDKeyAdaptResult);
  if PIN = '' then
  begin
    Result.Message := 'BMW ISN (Individual Serial Number) required';
    Exit;
  end;
  if Length(PIN) <> 32 then
  begin
    Result.Message :=
      'BMW ISN must be 32 hex chars (got ' +
      IntToStr(Length(PIN)) + ')';
    Exit;
  end;
  try
    Req := MakeOBDRequest;
    Req.ServiceID := $31;
    Req.Data := TBytes.Create(
      $01,
      Hi(ROUTINE_BMW_ADD_KEY),
      Lo(ROUTINE_BMW_ADD_KEY)
    ) + TEncoding.ASCII.GetBytes(PIN);
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('FEM add-key NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationBMW.DoClearOneSlot(
  ASlotIndex: Byte): TOBDKeyAdaptResult;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
begin
  Result := Default(TOBDKeyAdaptResult);
  Result.SlotIndex := ASlotIndex;
  // FEM exposes a per-slot disable via routine 0x4003 with a
  // single slot-index byte payload.
  try
    Req := MakeOBDRequest;
    Req.ServiceID := $31;
    Req.Data := TBytes.Create(
      $01, $40, $03, ASlotIndex
    );
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('FEM clear-slot NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationBMW.DoClearAllKeys: TOBDKeyAdaptResult;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
begin
  Result := Default(TOBDKeyAdaptResult);
  try
    Req := MakeOBDRequest;
    Req.ServiceID := $31;
    Req.Data := TBytes.Create(
      $01,
      Hi(ROUTINE_BMW_CLEAR_KEYS),
      Lo(ROUTINE_BMW_CLEAR_KEYS)
    );
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('FEM clear-all NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationBMW.DoCheckPin: TOBDKeyAdaptResult;
begin
  Result := Default(TOBDKeyAdaptResult);
  if Length(PIN) <> 32 then
  begin
    Result.Message :=
      'BMW ISN must be 32 hex chars (got ' +
      IntToStr(Length(PIN)) + ')';
    Exit;
  end;
  Result.Success := True;
end;

end.
