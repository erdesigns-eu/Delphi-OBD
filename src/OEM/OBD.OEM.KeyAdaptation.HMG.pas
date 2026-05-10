//------------------------------------------------------------------------------
//  OBD.OEM.KeyAdaptation.HMG
//
//  Hyundai / Kia / Genesis SMK (Smart Key Module) adaptation.
//  Drives the documented GDS-style procedure: enter the
//  4 / 6-digit PIN -> SecurityAccess seed/key on the SMK ECU
//  -> routine 0x0202 (clear keys) / 0x0201 (add key in
//  learning mode) at the SMK / immobiliser ECU.
//
//  Routine IDs sourced from public GDS-Mobile traces and
//  hyundai-gds.com / mhhauto.com community decode notes.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.OEM.KeyAdaptation.HMG;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Protocol.Types,
  OBD.OEM.KeyAdaptation.Types,
  OBD.OEM.KeyAdaptation.Base;

type
  TOBDKeyAdaptationHMG = class(TOBDKeyAdaptationBase)
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
  ROUTINE_HMG_ADD_KEY    = $0201;
  ROUTINE_HMG_CLEAR_KEYS = $0202;
  // SMK reads the slot-fill bitmap via DID 0xF18C.
  DID_HMG_SLOT_BITMAP    = $F18C;

function TOBDKeyAdaptationHMG.RequiresChassisCode: Boolean;
begin
  Result := True;
end;

function TOBDKeyAdaptationHMG.DoListSlots: TOBDKeyAdaptResult;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
  I:    Integer;
  Bitmap: Byte;
begin
  Result := Default(TOBDKeyAdaptResult);
  try
    Req := MakeOBDRequest;
    Req.ServiceID := $22;
    Req.Data := TBytes.Create(Hi(DID_HMG_SLOT_BITMAP),
                              Lo(DID_HMG_SLOT_BITMAP));
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('SMK NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    if Length(Resp.Data) < 1 then
    begin
      Result.Message := 'Truncated SMK slot-bitmap response';
      Exit;
    end;
    // SMK supports up to 4 keys; bits 0..3 set = filled.
    Bitmap := Resp.Data[0];
    SetLength(Result.Slots, 4);
    for I := 0 to 3 do
    begin
      Result.Slots[I].Index  := I + 1;
      Result.Slots[I].Filled := (Bitmap and (1 shl I)) <> 0;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationHMG.DoAddKey: TOBDKeyAdaptResult;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
  PinBytes: TBytes;
begin
  Result := Default(TOBDKeyAdaptResult);
  if PIN = '' then
  begin
    Result.Message := 'PIN required';
    Exit;
  end;
  // PIN as ASCII digits passed verbatim - SMK validates.
  PinBytes := TEncoding.ASCII.GetBytes(PIN);
  try
    Req := MakeOBDRequest;
    Req.ServiceID := $31;
    Req.Data := TBytes.Create(
      $01,
      Hi(ROUTINE_HMG_ADD_KEY),
      Lo(ROUTINE_HMG_ADD_KEY)
    ) + PinBytes;
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('SMK add-key NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationHMG.DoClearOneSlot(
  ASlotIndex: Byte): TOBDKeyAdaptResult;
begin
  Result := Default(TOBDKeyAdaptResult);
  Result.SlotIndex := ASlotIndex;
  Result.Message :=
    'HMG SMK does not expose per-slot delete - use ClearAllKeys';
end;

function TOBDKeyAdaptationHMG.DoClearAllKeys: TOBDKeyAdaptResult;
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
      Hi(ROUTINE_HMG_CLEAR_KEYS),
      Lo(ROUTINE_HMG_CLEAR_KEYS)
    ) + TEncoding.ASCII.GetBytes(PIN);
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('SMK clear-all NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationHMG.DoCheckPin: TOBDKeyAdaptResult;
begin
  Result := Default(TOBDKeyAdaptResult);
  if PIN = '' then
  begin
    Result.Message := 'PIN required';
    Exit;
  end;
  if not ((Length(PIN) = 4) or (Length(PIN) = 6)) then
  begin
    Result.Message :=
      'HMG PIN must be 4 or 6 digits (got ' +
      IntToStr(Length(PIN)) + ')';
    Exit;
  end;
  Result.Success := True;
end;

end.
