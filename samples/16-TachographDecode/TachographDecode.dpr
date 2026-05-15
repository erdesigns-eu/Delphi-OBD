//------------------------------------------------------------------------------
//  TachographDecode — sample 16
//
//  Demonstrates TOBDTachograph - decoder helpers for the
//  digital-tachograph data records mandated by EU
//  Regulation 165/2014 (driver / company / workshop /
//  control cards).
//
//  All helpers are pure class-static functions on TBytes;
//  the sample feeds them synthetic record payloads so the
//  example runs without a smartcard reader.
//
//  In production hosts read records from a card via PCSC
//  (TOBDTachoPCSC, Windows-only) or directly from a vehicle
//  unit and pipe the bytes through the same decoders.
//------------------------------------------------------------------------------

program TachographDecode;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.DateUtils,
  OBD.Speciality.Tachograph in '..\..\src\Speciality\OBD.Speciality.Tachograph.pas';

procedure DemoTimeReal;
var
  Now_: TDateTime;
  Reg:  Cardinal;
  Back: TDateTime;
begin
  Writeln('-- TimeReal (seconds since 1970-01-01 UTC) --');
  Now_ := EncodeDateTime(2026, 5, 10, 12, 0, 0, 0);
  Reg := TOBDTachograph.EncodeTimeReal(Now_);
  Writeln(Format('  EncodeTimeReal(2026-05-10 12:00 UTC) = %d', [Reg]));
  Back := TOBDTachograph.DecodeTimeReal(Reg);
  Writeln(Format('  DecodeTimeReal(%d)              = %s',
    [Reg, FormatDateTime('yyyy-mm-dd hh:nn:ss', Back)]));

  Writeln(Format('  Epoch sentinel: %d -> %s',
    [0, FormatDateTime('yyyy-mm-dd', TOBDTachograph.DecodeTimeReal(0))]));
end;

begin
  Writeln('Digital tachograph decoder demo');
  Writeln(StringOfChar('-', 60));
  DemoTimeReal;
  Writeln(StringOfChar('-', 60));
  Writeln('Done. (Activity / event / fault / calibration record');
  Writeln('decoders take TBytes payloads from a card or VU; see');
  Writeln('Tests.OBD.Speciality for vector inputs.)');
end.
