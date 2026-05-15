//------------------------------------------------------------------------------
//  OBD.OEM.KeyAdaptation.Toyota
//
//  Toyota / Lexus smart-key (SKID / SmartKey ECU) adaptation.
//
//  Two procedures:
//    - Master-key timing dance (older platforms; pre-2016
//      Auris / Camry / RAV4 etc.) - the host needs the
//      "all-keys-lost" master key to authorise.
//    - PIN-required smart-key add via SK ECU routine 0x0301
//      (post-2016 Lexus / Toyota with smart key).
//
//  Routine IDs sourced from Techstream / OPCOM-Toyota community
//  decode notes. ChassisCode lookup against
//  catalogs/key-platforms-toyota.json picks the variant.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.OEM.KeyAdaptation.Toyota;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Protocol.Types,
  OBD.OEM.KeyAdaptation.Types,
  OBD.OEM.KeyAdaptation.Base;

type
  TOBDKeyAdaptationToyota = class(TOBDKeyAdaptationBase)
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
  ROUTINE_SK_ADD          = $0301;
  ROUTINE_SK_DELETE_ALL   = $0302;
  // Smart-key slot-fill bitmap.
  DID_SK_SLOT_BITMAP      = $A101;

function TOBDKeyAdaptationToyota.RequiresChassisCode: Boolean;
begin
  Result := True;
end;

function TOBDKeyAdaptationToyota.DoListSlots: TOBDKeyAdaptResult;
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
    Req.Data := TBytes.Create(Hi(DID_SK_SLOT_BITMAP),
                              Lo(DID_SK_SLOT_BITMAP));
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('SK NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    if Length(Resp.Data) < 1 then
    begin
      Result.Message := 'Truncated SK slot-bitmap response';
      Exit;
    end;
    // Toyota SK supports up to 6 keys; bits 0..5 set = filled.
    Bitmap := Resp.Data[0];
    SetLength(Result.Slots, 6);
    for I := 0 to 5 do
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

function TOBDKeyAdaptationToyota.DoAddKey: TOBDKeyAdaptResult;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
begin
  Result := Default(TOBDKeyAdaptResult);
  if PIN = '' then
  begin
    Result.Message := 'PIN (master / smart-key seed) required';
    Exit;
  end;
  try
    Req := MakeOBDRequest;
    Req.ServiceID := $31;
    Req.Data := TBytes.Create(
      $01,
      Hi(ROUTINE_SK_ADD),
      Lo(ROUTINE_SK_ADD)
    ) + TEncoding.ASCII.GetBytes(PIN);
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('SK add-key NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationToyota.DoClearOneSlot(
  ASlotIndex: Byte): TOBDKeyAdaptResult;
begin
  Result := Default(TOBDKeyAdaptResult);
  Result.SlotIndex := ASlotIndex;
  Result.Message :=
    'Toyota SK does not expose per-slot delete - use ClearAllKeys';
end;

function TOBDKeyAdaptationToyota.DoClearAllKeys: TOBDKeyAdaptResult;
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
      Hi(ROUTINE_SK_DELETE_ALL),
      Lo(ROUTINE_SK_DELETE_ALL)
    ) + TEncoding.ASCII.GetBytes(PIN);
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('SK delete-all NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationToyota.DoCheckPin: TOBDKeyAdaptResult;
begin
  Result := Default(TOBDKeyAdaptResult);
  if PIN = '' then
  begin
    Result.Message := 'PIN required';
    Exit;
  end;
  // Toyota master-key seed is typically 8 hex chars; smart-key
  // certificate is 16. Accept either.
  if not ((Length(PIN) = 8) or (Length(PIN) = 16)) then
  begin
    Result.Message :=
      'Toyota PIN must be 8 (master-key) or 16 (smart-key cert) ' +
      'chars (got ' + IntToStr(Length(PIN)) + ')';
    Exit;
  end;
  Result.Success := True;
end;

end.
