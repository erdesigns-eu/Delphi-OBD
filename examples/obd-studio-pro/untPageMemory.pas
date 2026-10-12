//------------------------------------------------------------------------------
//  untPageMemory
//
//  EEPROM memory page for OBD Studio Pro: a deterministic 24C32 dump,
//  catalogue regions, checksum information and guarded read/write actions.
//
//    - TOBDHexView shows the dump and highlighted VIN, radio-code,
//      immobiliser, mileage and checksum regions.
//    - TOBDCard, TOBDSegmented, TOBDEdit, TOBDButton,
//      TOBDPreconditionList, TOBDProgressBar and TOBDListBox make the
//      read/write side panel.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untPageMemory;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Math,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  ERD.UI.Types, ERD.UI.Paint, ERD.UI.Toast, ERD.UI.Dialogs,
  ERD.UI.Card, ERD.UI.Buttons, ERD.UI.Edits, ERD.UI.Segmented,
  ERD.UI.Progress, ERD.UI.PreconditionList, ERD.UI.HexView,
  ERD.UI.ItemList,
  untProPage, untSimulator;

type
  TfrmMemory = class(TFrame, IProPage)
    hexDump: TOBDHexView;
    pnlSide: TPanel;
    cdMemoryActions: TOBDCard;
    segMemorySource: TOBDSegmented;
    edMemoryFile: TOBDEdit;
    edMemoryFrom: TOBDEdit;
    edMemoryTo: TOBDEdit;
    pbReadMemory: TOBDProgressBar;
    btnReadMemory: TOBDButton;
    btnWriteMemory: TOBDButton;
    btnSaveMemory: TOBDButton;
    btnCompareMemory: TOBDButton;
    preMemoryChecks: TOBDPreconditionList;
    lstMemoryInfo: TOBDListBox;
    procedure btnReadMemoryClick(Sender: TObject);
    procedure btnWriteMemoryClick(Sender: TObject);
    procedure btnSaveMemoryClick(Sender: TObject);
    procedure btnCompareMemoryClick(Sender: TObject);
  private
    FHost: IProHost;
    FReadActive: Boolean;
    FReadTicks: Integer;
    FDump: TBytes;
    FCompare: TBytes;
    function BuildDump: TBytes;
    function BuildCompareDump(const ASource: TBytes): TBytes;
    procedure LoadDump;
    procedure SetSideInfo(const AStatus: string);
  public
    procedure Init(const AHost: IProHost);
    procedure Tick;
    procedure ConnectionChanged;
  end;

implementation

{$R *.dfm}

const
  EEPROM_SIZE = 4096;
  MEMORY_READ_TICKS = 36;

function TfrmMemory.BuildCompareDump(const ASource: TBytes): TBytes;
begin
  Result := Copy(ASource);
  if Length(Result) > $009C then
  begin
    Result[$0052] := Result[$0052] xor $11;
    Result[$0053] := Result[$0053] xor $22;
    Result[$007A] := Result[$007A] xor $08;
    Result[$009C] := Result[$009C] xor $40;
  end;
end;

function TfrmMemory.BuildDump: TBytes;
var
  I: Integer;
  V: Int64;
  Vin: AnsiString;
begin
  SetLength(Result, EEPROM_SIZE);
  V := 7;
  for I := 0 to High(Result) do
  begin
    V := (V * 1103515245 + 12345) and $7FFFFFFF;
    Result[I] := Byte((V shr 16) and $FF);
  end;

  Vin := 'WVWZZZAUZGW123456';
  for I := 1 to Length(Vin) do
    Result[I - 1] := Ord(Vin[I]);
  Result[$0011] := $FF;
  Result[$0012] := $FF;
  Result[$0038] := $04;
  Result[$0039] := $01;
  Result[$003A] := $07;
  Result[$003B] := $02;
  Result[$0090] := $00;
  Result[$0091] := $02;
  Result[$0092] := $41;
  Result[$0093] := $86;
  Result[$00A0] := $49;
  Result[$00A1] := $44;
  Result[$00A2] := $34;
  Result[$00A3] := $38;
  Result[$0FFE] := $5A;
  Result[$0FFF] := $3C;
end;

procedure TfrmMemory.btnCompareMemoryClick(Sender: TObject);
begin
  hexDump.CompareData := FCompare;
  SetSideInfo('Backup compare enabled: 4 differing bytes.');
  if FHost <> nil then
    FHost.Notify(tkInfo, 'Compare enabled',
      'The EEPROM view is comparing against Backup 18:51 (.bin).');
end;

procedure TfrmMemory.btnReadMemoryClick(Sender: TObject);
begin
  if FReadActive then
    Exit;

  FReadActive := True;
  FReadTicks := 0;
  pbReadMemory.Position := 0;
  pbReadMemory.State := pbsNormal;
  btnReadMemory.Enabled := False;
  SetSideInfo('Reading 24C32 over KWP1281 address 0x56...');
  if FHost <> nil then
  begin
    FHost.Log('EEPROM read started for radio 56 / RCD 330 Delphi.');
    FHost.SetProgress('Reading EEPROM 24C32', 0);
  end;
end;

procedure TfrmMemory.btnSaveMemoryClick(Sender: TObject);
begin
  SetSideInfo('Saved as RCD330_24C32_2026-10-12.bin.');
  if FHost <> nil then
    FHost.Notify(tkSuccess, 'Dump saved',
      'The simulated EEPROM dump was attached to the Garage Peeters job.');
end;

procedure TfrmMemory.btnWriteMemoryClick(Sender: TObject);
begin
  if (FHost <> nil) and not FHost.Confirm(dkDanger, 'Write EEPROM',
    'Write the edited 24C32 image back to the radio? A backup is required before a real write.',
    'Write') then
    Exit;

  SetSideInfo('Write request simulated only; no EEPROM bytes were changed.');
  if FHost <> nil then
  begin
    FHost.LogWarning('EEPROM write was simulated; production write is disabled in the example.');
    FHost.Notify(tkWarning, 'Write simulated',
      'The example shows the guarded flow but does not alter the dump.');
  end;
end;

procedure TfrmMemory.ConnectionChanged;
begin
  btnReadMemory.Enabled := not FReadActive;
end;

procedure TfrmMemory.Init(const AHost: IProHost);
begin
  FHost := AHost;
  FDump := BuildDump;
  FCompare := BuildCompareDump(FDump);
  LoadDump;
  SetSideInfo('Ready: 24C32 dump loaded from the sample Golf VII job.');
  ConnectionChanged;
end;

procedure TfrmMemory.LoadDump;
var
  Sel: TOBDHexSelection;
begin
  hexDump.Clear;
  hexDump.Title := 'EEPROM 24C32 dump';
  hexDump.SetData(FDump);
  hexDump.CompareData := FCompare;
  hexDump.AddRegion('VIN WVWZZZAUZGW123456', $0000, 17, hvrNeutral);
  hexDump.AddRegion('Radio SAFE code bytes', $0038, 4, hvrAccent);
  hexDump.AddRegion('Odometer copy', $0090, 8, hvrWarning);
  hexDump.AddRegion('Immobilizer ID48', $00A0, 8, hvrSuccess);
  hexDump.AddRegion('Checksum Sum16', $0FFE, 2, hvrSuccess);
  Sel.Offset := $003A;
  Sel.Length := 2;
  hexDump.Selection := Sel;
end;

procedure TfrmMemory.SetSideInfo(const AStatus: string);
begin
  lstMemoryInfo.Items.BeginUpdate;
  try
    lstMemoryInfo.Items.Clear;
    lstMemoryInfo.Items.Add('Chip: 24C32 serial EEPROM / 4096 bytes');
    lstMemoryInfo.Items.Add('Vehicle: 2016 VW Golf VII 1.6 TDI CR');
    lstMemoryInfo.Items.Add('VIN: WVWZZZAUZGW123456');
    lstMemoryInfo.Items.Add('Radio: RCD 330 Delphi / address 0x56');
    lstMemoryInfo.Items.Add('Checksum: stored 5A3C / calculated 5A3C');
    lstMemoryInfo.Items.Add('Regions: VIN, radio code, mileage, immobilizer');
    lstMemoryInfo.Items.Add('Status: ' + AStatus);
  finally
    lstMemoryInfo.Items.EndUpdate;
  end;
end;

procedure TfrmMemory.Tick;
var
  Percent: Integer;
begin
  if not FReadActive then
    Exit;

  Inc(FReadTicks);
  Percent := EnsureRange(Round(FReadTicks * 100 / MEMORY_READ_TICKS), 0, 100);
  pbReadMemory.Position := Percent;
  if FHost <> nil then
    FHost.SetProgress('Reading EEPROM 24C32', Percent);

  if FReadTicks >= MEMORY_READ_TICKS then
  begin
    FReadActive := False;
    btnReadMemory.Enabled := True;
    pbReadMemory.State := pbsSuccess;
    LoadDump;
    SetSideInfo('Read complete: 4096 bytes, checksum OK.');
    if FHost <> nil then
    begin
      FHost.SetProgress('', -1);
      FHost.Log('EEPROM read complete: 4096 bytes, checksum 0x5A3C OK.');
      FHost.Notify(tkSuccess, 'EEPROM read complete',
        '24C32 dump captured and catalogue regions highlighted.');
    end;
  end;
end;

end.
