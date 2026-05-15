//------------------------------------------------------------------------------
//  OBD.OEM.KeyAdaptation.Ford
//
//  Ford PATS (Passive Anti-Theft System) key adaptation.
//  Drives the documented FORScan procedure: enter the
//  4-/8-digit incode, request the outcode, send routine
//  0xB003 / 0xB004 (PATS - delete keys, add new key) at the
//  IPC ECU.
//
//  Routine IDs and DID writes are taken from the public
//  FORScan / Forscan-Lite documentation; chassis-platform
//  applicability lives in catalogs/key-platforms-ford.json.
//
//  Important: Ford PATS gateway-locked platforms (post-2021
//  on most lines) require a SecOC-signed message that this
//  component does NOT generate. Call ListSlots first; the
//  result will surface "GatewayLocked" via the platform
//  catalogue, and the host should refuse to call AddKey on
//  those.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.OEM.KeyAdaptation.Ford;

interface

uses
  System.SysUtils,
  System.Classes,
  OBD.Protocol.Types,
  OBD.OEM.KeyAdaptation.Types,
  OBD.OEM.KeyAdaptation.Base;

type
  TOBDKeyAdaptationFord = class(TOBDKeyAdaptationBase)
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
  // PATS routine IDs (UDS Service 0x31). Sourced from public
  // FORScan documentation.
  ROUTINE_PATS_DELETE_KEYS = $B003;
  ROUTINE_PATS_ADD_KEY     = $B004;

function TOBDKeyAdaptationFord.RequiresChassisCode: Boolean;
begin
  Result := True;
end;

function TOBDKeyAdaptationFord.DoListSlots: TOBDKeyAdaptResult;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
  I:    Integer;
begin
  Result := Default(TOBDKeyAdaptResult);
  // PATS slot list = read DID 0xDD01 (number of programmed
  // keys) + iterate per-slot status DIDs 0xDD02..0xDD09. The
  // exact DIDs vary per platform; this implementation reads
  // the standard FORScan-documented DID 0xDD01 and reports
  // a flat slot count.
  try
    Req := MakeOBDRequest;
    Req.ServiceID := $22;
    Req.Data := TBytes.Create($DD, $01);
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    if Length(Resp.Data) < 1 then
    begin
      Result.Message := 'Truncated PATS slot-count response';
      Exit;
    end;
    SetLength(Result.Slots, Resp.Data[0]);
    for I := 0 to High(Result.Slots) do
    begin
      Result.Slots[I].Index  := I + 1;
      Result.Slots[I].Filled := True;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationFord.DoAddKey: TOBDKeyAdaptResult;
var
  Req:  TOBDRequest;
  Resp: TOBDResponse;
begin
  Result := Default(TOBDKeyAdaptResult);
  if PIN = '' then
  begin
    Result.Message := 'PIN (incode/outcode) required';
    Exit;
  end;
  try
    // Routine 0xB004 with the supplied incode bytes.
    Req := MakeOBDRequest;
    Req.ServiceID := $31;
    Req.Data := TBytes.Create(
      $01,                              // start routine
      Hi(ROUTINE_PATS_ADD_KEY),
      Lo(ROUTINE_PATS_ADD_KEY)
    ) + TEncoding.ASCII.GetBytes(PIN);
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('PATS add-key NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationFord.DoClearOneSlot(
  ASlotIndex: Byte): TOBDKeyAdaptResult;
begin
  Result := Default(TOBDKeyAdaptResult);
  Result.SlotIndex := ASlotIndex;
  // PATS doesn't expose per-slot delete on most platforms;
  // delete-all + re-program is the documented procedure.
  Result.Message :=
    'Ford PATS does not expose per-slot delete on this ' +
    'platform - use ClearAllKeys + AddKey for each remaining key';
end;

function TOBDKeyAdaptationFord.DoClearAllKeys: TOBDKeyAdaptResult;
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
      Hi(ROUTINE_PATS_DELETE_KEYS),
      Lo(ROUTINE_PATS_DELETE_KEYS)
    );
    Resp := RequireProtocol.Send(Req);
    if Resp.IsNegative then
    begin
      Result.Message := Format('PATS delete-all NRC 0x%.2X - %s',
        [Resp.NRC, Resp.NRCText]);
      Exit;
    end;
    Result.Success := True;
  except
    on E: Exception do
      Result.Message := E.ClassName + ': ' + E.Message;
  end;
end;

function TOBDKeyAdaptationFord.DoCheckPin: TOBDKeyAdaptResult;
begin
  Result := Default(TOBDKeyAdaptResult);
  if PIN = '' then
  begin
    Result.Message := 'PIN required';
    Exit;
  end;
  // Validate PIN length: Ford PATS uses 4 hex pairs
  // (8 chars) for outcode-style platforms.
  if not ((Length(PIN) = 4) or (Length(PIN) = 8)) then
  begin
    Result.Message :=
      'Ford PATS PIN must be 4 or 8 chars (got ' +
      IntToStr(Length(PIN)) + ')';
    Exit;
  end;
  Result.Success := True;
end;

end.
