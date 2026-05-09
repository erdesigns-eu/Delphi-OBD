//------------------------------------------------------------------------------
// UNIT           : OBD.J1939.PGNs.pas
// CONTENTS       : SAE J1939-71/73/75 named-PGN catalog
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows / macOS / Linux / iOS / Android
// RELEASE DATE   : 09/05/2026
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.J1939.PGNs;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults;

type
  TJ1939PGNDescriptor = record
    PGN: UInt32;
    Mnemonic: string;       // e.g. 'EEC1', 'DM1'
    Name: string;           // e.g. 'Electronic Engine Controller 1'
    LengthBytes: Integer;   // 0 = variable / multi-packet
    DefaultPriority: Byte;  // 0..7 (0 highest)
    TxRateMs: Integer;      // 0 = on request only; -1 = on change only
    SpecSection: string;    // 'J1939-71 §5.3.1', 'J1939-73 §5.7.1', etc.
  end;

/// <summary>Look up a PGN by id. Returns a zero record (PGN = 0) when
/// not found; callers can check Result.PGN <> 0.</summary>
function FindPGN(const PGN: UInt32): TJ1939PGNDescriptor;

/// <summary>Register a custom PGN (e.g. for OEM-specific extensions).
/// Replaces an existing entry with the same id.</summary>
procedure RegisterJ1939PGN(const Desc: TJ1939PGNDescriptor);

/// <summary>Total entries in the registry (built-in + registered).</summary>
function J1939PGNCount: Integer;

/// <summary>Iterate all PGNs in ascending order.</summary>
function J1939PGNAll: TArray<TJ1939PGNDescriptor>;

implementation

var
  GPGNs: TList<TJ1939PGNDescriptor>;

procedure SeedPGN(PGN: UInt32; const Mnem, Name: string; LenBytes: Integer;
  Pri: Byte; TxRate: Integer; const Spec: string);
var
  D: TJ1939PGNDescriptor;
begin
  D.PGN := PGN;
  D.Mnemonic := Mnem;
  D.Name := Name;
  D.LengthBytes := LenBytes;
  D.DefaultPriority := Pri;
  D.TxRateMs := TxRate;
  D.SpecSection := Spec;
  GPGNs.Add(D);
end;

procedure SeedDefaults;
begin
  // ---- Powertrain (J1939-71 §5.3) ------------------------------------
  SeedPGN($F004, 'EEC1', 'Electronic Engine Controller 1',                8, 3,   20, 'J1939-71 §5.3.1');
  SeedPGN($F003, 'EEC2', 'Electronic Engine Controller 2',                8, 3,   50, 'J1939-71 §5.3.2');
  SeedPGN($FEDF, 'EEC3', 'Electronic Engine Controller 3',                8, 6,  250, 'J1939-71 §5.3.3');
  SeedPGN($FE9E, 'EEC4', 'Electronic Engine Controller 4',                8, 3,  100, 'J1939-71 §5.3.4');
  SeedPGN($FEEE, 'ET1',  'Engine Temperature 1',                           8, 6, 1000, 'J1939-71 §5.3.6');
  SeedPGN($FEEF, 'EFL/P1','Engine Fluid Level/Pressure 1',                  8, 6,  500, 'J1939-71 §5.3.7');
  SeedPGN($FEF2, 'LFE1', 'Fuel Economy (Liquid)',                          8, 6,  100, 'J1939-71 §5.3.8');
  SeedPGN($FEF1, 'CCVS', 'Cruise Control / Vehicle Speed',                 8, 6,  100, 'J1939-71 §5.3.9');
  SeedPGN($FEF5, 'AMB',  'Ambient Conditions',                              8, 6, 1000, 'J1939-71 §5.3.10');
  SeedPGN($FEF6, 'IC1',  'Inlet/Exhaust Conditions 1',                     8, 6,  500, 'J1939-71 §5.3.11');
  SeedPGN($FEF7, 'VEP1', 'Vehicle Electrical Power 1',                     8, 6, 1000, 'J1939-71 §5.3.12');
  SeedPGN($FEF8, 'TRF1', 'Transmission Fluids 1',                          8, 6, 1000, 'J1939-71 §5.3.13');
  SeedPGN($FEFC, 'DD',   'Dash Display',                                   8, 6, 1000, 'J1939-71 §5.3.14');
  SeedPGN($FEFE, 'AAI',  'Auxiliary Analog Information',                   8, 6, 1000, 'J1939-71 §5.3.15');
  SeedPGN($FEFF, 'WFI',  'Water in Fuel Indicator',                        8, 6, 1000, 'J1939-71 §5.3.16');
  SeedPGN($FECA, 'DM1',  'Active Diagnostic Trouble Codes',                0, 6,    0, 'J1939-73 §5.7.1');
  SeedPGN($FECB, 'DM2',  'Previously Active DTCs',                         0, 6,    0, 'J1939-73 §5.7.2');
  SeedPGN($FECC, 'DM3',  'Diagnostic Data Clear (Previously Active)',      0, 6,    0, 'J1939-73 §5.7.3');
  SeedPGN($FECD, 'DM4',  'Freeze Frame Parameters',                        0, 6,    0, 'J1939-73 §5.7.4');
  SeedPGN($FECE, 'DM5',  'Diagnostic Readiness 1',                         8, 6,    0, 'J1939-73 §5.7.5');
  SeedPGN($FED3, 'DM11', 'Diagnostic Data Clear (Active)',                 0, 6,    0, 'J1939-73 §5.7.11');
  SeedPGN($FED5, 'DM12', 'Emission-Related Active DTCs',                   0, 6,    0, 'J1939-73 §5.7.12');
  SeedPGN($FECF, 'DM6',  'Emission-Related Pending DTCs',                  0, 6,    0, 'J1939-73 §5.7.6');
  SeedPGN($FE2A, 'DM7',  'Test Results',                                   0, 6,    0, 'J1939-73 §5.7.7');
  SeedPGN($FE2B, 'DM8',  'Test Results — broadcast',                       0, 6,    0, 'J1939-73 §5.7.8');
  SeedPGN($FE2C, 'DM10', 'Inactive DTCs Selected',                         0, 6,    0, 'J1939-73 §5.7.10');
  SeedPGN($FDB0, 'DM23', 'Emission-Related Previously Active DTCs',        0, 6,    0, 'J1939-73 §5.7.23');
  SeedPGN($FE6F, 'DM26', 'Diagnostic Readiness 3',                         8, 6,    0, 'J1939-73 §5.7.26');

  // ---- Brakes (J1939-71 §5.4) ----------------------------------------
  SeedPGN($FEAE, 'AIR1', 'Air Supply Pressure',                            8, 6, 1000, 'J1939-71 §5.4.1');
  SeedPGN($F001, 'EBC1', 'Electronic Brake Controller 1',                  8, 3,  100, 'J1939-71 §5.4.2');
  SeedPGN($FEC1, 'HRVD', 'High Resolution Vehicle Distance',               8, 6,  250, 'J1939-71 §5.4.4');
  SeedPGN($FEC4, 'EBS5', 'Electronic Brake Stability',                     8, 3,   20, 'J1939-71 §5.4.5');

  // ---- Transmission (J1939-71 §5.5) ----------------------------------
  SeedPGN($F002, 'ETC1', 'Electronic Transmission Controller 1',           8, 3,   10, 'J1939-71 §5.5.1');
  SeedPGN($F005, 'ETC2', 'Electronic Transmission Controller 2',           8, 3,  100, 'J1939-71 §5.5.2');
  SeedPGN($FFEC, 'ETC3', 'Electronic Transmission Controller 3',           8, 6,  250, 'J1939-71 §5.5.3');
  SeedPGN($FF00, 'ETC7', 'Electronic Transmission Controller 7',           8, 3,  100, 'J1939-71 §5.5.7');

  // ---- Body & Cab (J1939-71 §5.6) ------------------------------------
  SeedPGN($FEF0, 'PTO',  'Power Takeoff Information',                      8, 6,  100, 'J1939-71 §5.6.1');
  SeedPGN($FEF3, 'VP',   'Vehicle Position',                               8, 6, 5000, 'J1939-71 §5.6.2');
  SeedPGN($FEE9, 'TIME', 'Time / Date',                                    8, 6, 1000, 'J1939-71 §5.6.4');
  SeedPGN($FEEA, 'VW',   'Vehicle Weight',                                 8, 6,  500, 'J1939-71 §5.6.5');
  SeedPGN($FEEC, 'VI',   'Vehicle Identification (VIN)',                   0, 6,    0, 'J1939-71 §5.6.6');
  SeedPGN($FEEB, 'CI',   'Component Identification',                       0, 6,    0, 'J1939-71 §5.6.7');
  SeedPGN($FEE5, 'EH',   'Engine Hours / Revolutions',                     8, 6, 1000, 'J1939-71 §5.6.10');

  // ---- After-treatment (J1939-71 §5.7) -------------------------------
  SeedPGN($FE56, 'AT1IG1', 'After-treatment 1 Diesel Exhaust Fluid Tank 1',8, 6, 1000, 'J1939-71 §5.7.1');
  SeedPGN($FD7C, 'AT1S',   'After-treatment 1 Status (DPF/SCR)',           8, 6, 1000, 'J1939-71 §5.7.2');
  SeedPGN($FD7D, 'DPFC1',  'Diesel Particulate Filter Control 1',          8, 6, 1000, 'J1939-71 §5.7.3');
  SeedPGN($FE57, 'AT1IMG1','After-treatment 1 DEF Quality',                8, 6, 1000, 'J1939-71 §5.7.4');
  SeedPGN($FE5B, 'AT1OG1', 'After-treatment 1 Outlet Gas',                 8, 6, 1000, 'J1939-71 §5.7.5');

  // ---- Network management (J1939-21 / 81) ----------------------------
  SeedPGN($EE00, 'AC',    'Address Claimed / Cannot Claim',                8, 6,    0, 'J1939-81 §4.2');
  SeedPGN($EC00, 'TP.CM', 'Transport Protocol Connection Management',      8, 7,    0, 'J1939-21 §5.10.1');
  SeedPGN($EB00, 'TP.DT', 'Transport Protocol Data Transfer',              8, 7,    0, 'J1939-21 §5.10.2');

  // ---- Generator sets (J1939-75) -------------------------------------
  SeedPGN($FFC9, 'GG',    'Genset Group',                                  8, 6, 1000, 'J1939-75 §6.1');
  SeedPGN($FFC8, 'GAP',   'Genset Average Power',                          8, 6, 1000, 'J1939-75 §6.2');
  SeedPGN($FFC7, 'GTH',   'Genset Total Hours',                            8, 6, 1000, 'J1939-75 §6.3');
  SeedPGN($FFC6, 'GTHA',  'Genset Total Hours — Active',                   8, 6, 1000, 'J1939-75 §6.4');
end;

function FindPGNIndex(PGN: UInt32; out Idx: Integer): Boolean;
var
  Lo, Hi, Mid: Integer;
  V: UInt32;
begin
  Lo := 0;
  Hi := GPGNs.Count - 1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) shr 1;
    V := GPGNs[Mid].PGN;
    if V = PGN then
    begin
      Idx := Mid;
      Exit(True);
    end
    else if V < PGN then
      Lo := Mid + 1
    else
      Hi := Mid - 1;
  end;
  Idx := -1;
  Result := False;
end;

procedure SortBy_PGN;
begin
  GPGNs.Sort(TComparer<TJ1939PGNDescriptor>.Construct(
    function(const A, B: TJ1939PGNDescriptor): Integer
    begin
      if A.PGN < B.PGN then Result := -1
      else if A.PGN > B.PGN then Result := 1
      else Result := 0;
    end));
end;

function FindPGN(const PGN: UInt32): TJ1939PGNDescriptor;
var Idx: Integer;
begin
  if FindPGNIndex(PGN, Idx) then
    Result := GPGNs[Idx]
  else
    Result := Default(TJ1939PGNDescriptor);
end;

procedure RegisterJ1939PGN(const Desc: TJ1939PGNDescriptor);
var Idx: Integer;
begin
  if FindPGNIndex(Desc.PGN, Idx) then
    GPGNs[Idx] := Desc
  else
  begin
    GPGNs.Add(Desc);
    SortBy_PGN;
  end;
end;

function J1939PGNCount: Integer;
begin
  Result := GPGNs.Count;
end;

function J1939PGNAll: TArray<TJ1939PGNDescriptor>;
begin
  Result := GPGNs.ToArray;
end;

initialization
  GPGNs := TList<TJ1939PGNDescriptor>.Create;
  SeedDefaults;
  SortBy_PGN;

finalization
  GPGNs.Free;

end.
