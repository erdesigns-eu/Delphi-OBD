//------------------------------------------------------------------------------
//  OBD.Design.Starters.Templates
//
//  Concrete starter generators registered at design-time package
//  install. Adding another starter is a single block at the
//  bottom of this unit — define a generator and call
//  <c>TOBDStarterRegistry.Default.Register(…)</c>.
//
//  Conventions used by every starter:
//
//    - <c>{PROJ}</c> in templates is replaced with the project
//      name; <c>{UNIT}</c>, <c>{FORM}</c>, <c>{FORMVAR}</c>
//      with the unit, form-class and form-instance names.
//    - All starters produce a <c>.dpr</c> + one
//      <c>.pas</c>/<c>.dfm</c> pair.
//    - Components are declared inline in the <c>.dfm</c> so the
//      IDE wires them up the moment the project opens.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Starters.Templates;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Design.Starters;

{ ---- helpers ----------------------------------------------------------------
  Expand and MakeDpr are now public on OBD.Design.Starters; the
  shorthand aliases below keep this unit's bodies short. }

function Expand(const ATemplate: string;
  const AContext: TOBDStarterContext): string; inline;
begin
  Result := ExpandTemplate(ATemplate, AContext);
end;

function MakeDpr(const AContext: TOBDStarterContext): TOBDStarterArtifact; inline;
begin
  Result := MakeDprArtifact(AContext);
end;

{ ---- 01 — Empty (connection chain only) -------------------------------------- }

function GenerateEmpty(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10 +
    ''#13#10 +
    'interface'#13#10 +
    ''#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};'#13#10 +
    ''#13#10 +
    'implementation'#13#10 +
    ''#13#10 +
    '{$R *.dfm}'#13#10 +
    ''#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ}'''#13#10 +
    '  ClientHeight = 320'#13#10 +
    '  ClientWidth = 480'#13#10 +
    '  Position = poScreenCenter'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24'#13#10 +
    '    Top = 24'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection'#13#10 +
    '    Left = 88'#13#10 +
    '    Top = 24'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter'#13#10 +
    '    Left = 152'#13#10 +
    '    Top = 24'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1].RelativePath  := AContext.UnitName + '.pas';
  Result[1].Content       := Expand(PAS_TPL, AContext);
  Result[2].RelativePath  := AContext.UnitName + '.dfm';
  Result[2].Content       := Expand(DFM_TPL, AContext);
end;

{ ---- 02 — DTC reader / reset ------------------------------------------------- }

function GenerateDTC(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10 +
    ''#13#10 +
    'interface'#13#10 +
    ''#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Service.DTCs;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    DTCs:       TOBDDTCs;'#13#10 +
    '    pnlActions: TPanel;'#13#10 +
    '    btnRead:    TButton;'#13#10 +
    '    btnClear:   TButton;'#13#10 +
    '    lvDTCs:     TListView;'#13#10 +
    '    procedure btnReadClick(Sender: TObject);'#13#10 +
    '    procedure btnClearClick(Sender: TObject);'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};'#13#10 +
    ''#13#10 +
    'implementation'#13#10 +
    ''#13#10 +
    '{$R *.dfm}'#13#10 +
    ''#13#10 +
    'procedure {FORM}.btnReadClick(Sender: TObject);'#13#10 +
    'var'#13#10 +
    '  Codes: TArray<string>;'#13#10 +
    '  C: string;'#13#10 +
    '  Item: TListItem;'#13#10 +
    'begin'#13#10 +
    '  Connection.Open;'#13#10 +
    '  try'#13#10 +
    '    Adapter.Detect;'#13#10 +
    '    Codes := DTCs.Read;'#13#10 +
    '    lvDTCs.Items.BeginUpdate;'#13#10 +
    '    try'#13#10 +
    '      lvDTCs.Items.Clear;'#13#10 +
    '      for C in Codes do'#13#10 +
    '      begin'#13#10 +
    '        Item := lvDTCs.Items.Add;'#13#10 +
    '        Item.Caption := C;'#13#10 +
    '      end;'#13#10 +
    '    finally'#13#10 +
    '      lvDTCs.Items.EndUpdate;'#13#10 +
    '    end;'#13#10 +
    '  finally'#13#10 +
    '    Connection.Close;'#13#10 +
    '  end;'#13#10 +
    'end;'#13#10 +
    ''#13#10 +
    'procedure {FORM}.btnClearClick(Sender: TObject);'#13#10 +
    'begin'#13#10 +
    '  Connection.Open;'#13#10 +
    '  try'#13#10 +
    '    DTCs.Clear;'#13#10 +
    '    lvDTCs.Items.Clear;'#13#10 +
    '  finally'#13#10 +
    '    Connection.Close;'#13#10 +
    '  end;'#13#10 +
    'end;'#13#10 +
    ''#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - DTC reader'''#13#10 +
    '  ClientHeight = 420'#13#10 +
    '  ClientWidth = 600'#13#10 +
    '  Position = poScreenCenter'#13#10 +
    '  object pnlActions: TPanel'#13#10 +
    '    Align = alTop'#13#10 +
    '    Height = 48'#13#10 +
    '    BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12'#13#10 +
    '    Padding.Top = 10'#13#10 +
    '    Padding.Right = 12'#13#10 +
    '    Padding.Bottom = 8'#13#10 +
    '    object btnRead: TButton'#13#10 +
    '      Left = 12'#13#10 +
    '      Top = 10'#13#10 +
    '      Width = 110'#13#10 +
    '      Height = 28'#13#10 +
    '      Caption = ''Read DTCs'''#13#10 +
    '      OnClick = btnReadClick'#13#10 +
    '    end'#13#10 +
    '    object btnClear: TButton'#13#10 +
    '      Left = 130'#13#10 +
    '      Top = 10'#13#10 +
    '      Width = 110'#13#10 +
    '      Height = 28'#13#10 +
    '      Caption = ''Clear DTCs'''#13#10 +
    '      OnClick = btnClearClick'#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object lvDTCs: TListView'#13#10 +
    '    Align = alClient'#13#10 +
    '    Columns = <'#13#10 +
    '      item'#13#10 +
    '        Caption = ''Code'''#13#10 +
    '        Width = 120'#13#10 +
    '      end>'#13#10 +
    '    ViewStyle = vsReport'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24'#13#10 +
    '    Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection'#13#10 +
    '    Left = 88'#13#10 +
    '    Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter'#13#10 +
    '    Left = 152'#13#10 +
    '    Top = 80'#13#10 +
    '  end'#13#10 +
    '  object DTCs: TOBDDTCs'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    Left = 216'#13#10 +
    '    Top = 80'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1].RelativePath  := AContext.UnitName + '.pas';
  Result[1].Content       := Expand(PAS_TPL, AContext);
  Result[2].RelativePath  := AContext.UnitName + '.dfm';
  Result[2].Content       := Expand(DFM_TPL, AContext);
end;

{ ---- 03 — VIN reader --------------------------------------------------------- }

function GenerateVIN(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10 +
    ''#13#10 +
    'interface'#13#10 +
    ''#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Service.VIN;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    VIN:        TOBDVIN;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    btnRead:    TButton;'#13#10 +
    '    lblVIN:     TLabel;'#13#10 +
    '    procedure btnReadClick(Sender: TObject);'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};'#13#10 +
    ''#13#10 +
    'implementation'#13#10 +
    ''#13#10 +
    '{$R *.dfm}'#13#10 +
    ''#13#10 +
    'procedure {FORM}.btnReadClick(Sender: TObject);'#13#10 +
    'begin'#13#10 +
    '  Connection.Open;'#13#10 +
    '  try'#13#10 +
    '    Adapter.Detect;'#13#10 +
    '    lblVIN.Caption := VIN.Read;'#13#10 +
    '  finally'#13#10 +
    '    Connection.Close;'#13#10 +
    '  end;'#13#10 +
    'end;'#13#10 +
    ''#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - VIN reader'''#13#10 +
    '  ClientHeight = 200'#13#10 +
    '  ClientWidth = 480'#13#10 +
    '  Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alClient'#13#10 +
    '    BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 16'#13#10 +
    '    Padding.Top = 16'#13#10 +
    '    Padding.Right = 16'#13#10 +
    '    Padding.Bottom = 16'#13#10 +
    '    object btnRead: TButton'#13#10 +
    '      Align = alTop'#13#10 +
    '      Height = 32'#13#10 +
    '      Caption = ''Read VIN'''#13#10 +
    '      OnClick = btnReadClick'#13#10 +
    '    end'#13#10 +
    '    object lblVIN: TLabel'#13#10 +
    '      AlignWithMargins = True'#13#10 +
    '      Margins.Top = 16'#13#10 +
    '      Align = alClient'#13#10 +
    '      Alignment = taCenter'#13#10 +
    '      Caption = ''(VIN appears here)'''#13#10 +
    '      Layout = tlCenter'#13#10 +
    '      Font.Charset = DEFAULT_CHARSET'#13#10 +
    '      Font.Color = clWindowText'#13#10 +
    '      Font.Height = -16'#13#10 +
    '      Font.Name = ''Consolas'''#13#10 +
    '      Font.Style = []'#13#10 +
    '      ParentFont = False'#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24'#13#10 +
    '    Top = 16'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection'#13#10 +
    '    Left = 88'#13#10 +
    '    Top = 16'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter'#13#10 +
    '    Left = 152'#13#10 +
    '    Top = 16'#13#10 +
    '  end'#13#10 +
    '  object VIN: TOBDVIN'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    Left = 216'#13#10 +
    '    Top = 16'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := Expand(PAS_TPL, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := Expand(DFM_TPL, AContext);
end;

{ ---- 04 — Live data dashboard ------------------------------------------------ }

function GenerateLiveData(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10 +
    ''#13#10 +
    'interface'#13#10 +
    ''#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Service.LiveData,'#13#10 +
    '  OBD.Types;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    LiveData:   TOBDLiveData;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    btnStart:   TButton;'#13#10 +
    '    btnStop:    TButton;'#13#10 +
    '    grid:       TStringGrid;'#13#10 +
    '    procedure btnStartClick(Sender: TObject);'#13#10 +
    '    procedure btnStopClick(Sender: TObject);'#13#10 +
    '    procedure LiveDataValue(Sender: TObject; APid: Byte;'#13#10 +
    '      const AValue: TOBDValue);'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};'#13#10 +
    ''#13#10 +
    'implementation'#13#10 +
    ''#13#10 +
    '{$R *.dfm}'#13#10 +
    ''#13#10 +
    'procedure {FORM}.btnStartClick(Sender: TObject);'#13#10 +
    'begin'#13#10 +
    '  Connection.Open;'#13#10 +
    '  Adapter.Detect;'#13#10 +
    '  // Poll RPM (0x0C), speed (0x0D), coolant (0x05).'#13#10 +
    '  LiveData.Poll(TBytes.Create($0C, $0D, $05), 250);'#13#10 +
    'end;'#13#10 +
    ''#13#10 +
    'procedure {FORM}.btnStopClick(Sender: TObject);'#13#10 +
    'begin'#13#10 +
    '  LiveData.PollStop;'#13#10 +
    '  Connection.Close;'#13#10 +
    'end;'#13#10 +
    ''#13#10 +
    'procedure {FORM}.LiveDataValue(Sender: TObject; APid: Byte;'#13#10 +
    '  const AValue: TOBDValue);'#13#10 +
    'var'#13#10 +
    '  Row: Integer;'#13#10 +
    'begin'#13#10 +
    '  Row := APid mod grid.RowCount;'#13#10 +
    '  if Row < 1 then Row := 1;'#13#10 +
    '  grid.Cells[0, Row] := Format(''0x%.2X'', [APid]);'#13#10 +
    '  if AValue.Kind = vkFloat then'#13#10 +
    '    grid.Cells[1, Row] := Format(''%.2f %s'', [AValue.AsFloat, AValue.Units])'#13#10 +
    '  else'#13#10 +
    '    grid.Cells[1, Row] := AValue.AsString;'#13#10 +
    'end;'#13#10 +
    ''#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - live data'''#13#10 +
    '  ClientHeight = 420'#13#10 +
    '  ClientWidth = 600'#13#10 +
    '  Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop'#13#10 +
    '    Height = 48'#13#10 +
    '    BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12'#13#10 +
    '    Padding.Top = 10'#13#10 +
    '    Padding.Right = 12'#13#10 +
    '    Padding.Bottom = 8'#13#10 +
    '    object btnStart: TButton'#13#10 +
    '      Left = 12'#13#10 +
    '      Top = 10'#13#10 +
    '      Width = 90'#13#10 +
    '      Height = 28'#13#10 +
    '      Caption = ''Start'''#13#10 +
    '      OnClick = btnStartClick'#13#10 +
    '    end'#13#10 +
    '    object btnStop: TButton'#13#10 +
    '      Left = 110'#13#10 +
    '      Top = 10'#13#10 +
    '      Width = 90'#13#10 +
    '      Height = 28'#13#10 +
    '      Caption = ''Stop'''#13#10 +
    '      OnClick = btnStopClick'#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object grid: TStringGrid'#13#10 +
    '    Align = alClient'#13#10 +
    '    ColCount = 2'#13#10 +
    '    DefaultRowHeight = 22'#13#10 +
    '    FixedCols = 0'#13#10 +
    '    RowCount = 32'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24'#13#10 +
    '    Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection'#13#10 +
    '    Left = 88'#13#10 +
    '    Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter'#13#10 +
    '    Left = 152'#13#10 +
    '    Top = 80'#13#10 +
    '  end'#13#10 +
    '  object LiveData: TOBDLiveData'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    OnValue = LiveDataValue'#13#10 +
    '    Left = 216'#13#10 +
    '    Top = 80'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := Expand(PAS_TPL, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := Expand(DFM_TPL, AContext);
end;

{ ---- 05 — Coding session ----------------------------------------------------- }

function GenerateCoding(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10 +
    ''#13#10 +
    'interface'#13#10 +
    ''#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol,'#13#10 +
    '  OBD.Coding.SecurityAccess, OBD.Coding.DataIdentifierIO,'#13#10 +
    '  OBD.Coding.AuditLog, OBD.Coding.Session;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection:     TOBDConnection;'#13#10 +
    '    Adapter:        TOBDAdapter;'#13#10 +
    '    Protocol:       TOBDProtocol;'#13#10 +
    '    SecurityAccess: TOBDSecurityAccess;'#13#10 +
    '    DataIO:         TOBDDataIdentifierIO;'#13#10 +
    '    AuditLog:       TOBDCodingAuditLog;'#13#10 +
    '    Session:        TOBDCodingSession;'#13#10 +
    '    pnl:            TPanel;'#13#10 +
    '    edtDID:         TEdit;'#13#10 +
    '    btnRead:        TButton;'#13#10 +
    '    btnWrite:       TButton;'#13#10 +
    '    edtData:        TEdit;'#13#10 +
    '    memLog:         TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};'#13#10 +
    ''#13#10 +
    'implementation'#13#10 +
    ''#13#10 +
    '{$R *.dfm}'#13#10 +
    ''#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - coding session'''#13#10 +
    '  ClientHeight = 480'#13#10 +
    '  ClientWidth = 640'#13#10 +
    '  Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop'#13#10 +
    '    Height = 56'#13#10 +
    '    BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12'#13#10 +
    '    Padding.Top = 14'#13#10 +
    '    Padding.Right = 12'#13#10 +
    '    Padding.Bottom = 12'#13#10 +
    '    object edtDID: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 120; Height = 24'#13#10 +
    '      TextHint = ''DID hex (e.g. F190)'''#13#10 +
    '    end'#13#10 +
    '    object edtData: TEdit'#13#10 +
    '      Left = 144; Top = 14; Width = 240; Height = 24'#13#10 +
    '      TextHint = ''Data bytes hex (write only)'''#13#10 +
    '    end'#13#10 +
    '    object btnRead: TButton'#13#10 +
    '      Left = 396; Top = 12; Width = 100; Height = 28'#13#10 +
    '      Caption = ''Read DID'''#13#10 +
    '    end'#13#10 +
    '    object btnWrite: TButton'#13#10 +
    '      Left = 504; Top = 12; Width = 100; Height = 28'#13#10 +
    '      Caption = ''Write DID'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET'#13#10 +
    '    Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12'#13#10 +
    '    Font.Name = ''Consolas'''#13#10 +
    '    Font.Style = []'#13#10 +
    '    ParentFont = False'#13#10 +
    '    ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection'#13#10 +
    '    Left = 88; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter'#13#10 +
    '    Left = 152; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object SecurityAccess: TOBDSecurityAccess'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    Left = 216; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object DataIO: TOBDDataIdentifierIO'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    Left = 280; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object AuditLog: TOBDCodingAuditLog'#13#10 +
    '    Left = 344; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Session: TOBDCodingSession'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    DataIO   = DataIO'#13#10 +
    '    AuditLog = AuditLog'#13#10 +
    '    Left = 408; Top = 80'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := Expand(PAS_TPL, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := Expand(DFM_TPL, AContext);
end;

{ ---- 06 — Flash session ------------------------------------------------------ }

function GenerateFlash(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10 +
    ''#13#10 +
    'interface'#13#10 +
    ''#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,'#13#10 +
    '  Vcl.Dialogs,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol,'#13#10 +
    '  OBD.Coding.AuditLog, OBD.UDS.Transfer,'#13#10 +
    '  OBD.Flash.VoltageGate, OBD.Flash.Pipeline;'#13#10 +
    ''#13#10 +
    '// SAFETY: this starter wires the destructive flash pipeline.'#13#10 +
    '// Do NOT toggle AutoExecute = True until you have wired'#13#10 +
    '// OnConfirmExecute and read docs/flashing-safety.md.'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection:  TOBDConnection;'#13#10 +
    '    Adapter:     TOBDAdapter;'#13#10 +
    '    Protocol:    TOBDProtocol;'#13#10 +
    '    AuditLog:    TOBDCodingAuditLog;'#13#10 +
    '    UDSTransfer: TOBDUDSTransfer;'#13#10 +
    '    VoltageGate: TOBDVoltageGate;'#13#10 +
    '    Pipeline:    TOBDFlashPipeline;'#13#10 +
    '    pnl:         TPanel;'#13#10 +
    '    edtImage:    TEdit;'#13#10 +
    '    btnPick:     TButton;'#13#10 +
    '    btnRun:      TButton;'#13#10 +
    '    pb:          TProgressBar;'#13#10 +
    '    memLog:      TMemo;'#13#10 +
    '    dlg:         TOpenDialog;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};'#13#10 +
    ''#13#10 +
    'implementation'#13#10 +
    ''#13#10 +
    '{$R *.dfm}'#13#10 +
    ''#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - flash session'''#13#10 +
    '  ClientHeight = 520'#13#10 +
    '  ClientWidth = 700'#13#10 +
    '  Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop'#13#10 +
    '    Height = 80'#13#10 +
    '    BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12'#13#10 +
    '    Padding.Top = 12'#13#10 +
    '    Padding.Right = 12'#13#10 +
    '    Padding.Bottom = 12'#13#10 +
    '    object edtImage: TEdit'#13#10 +
    '      Left = 12; Top = 12; Width = 460; Height = 24'#13#10 +
    '      TextHint = ''Image file (.bin / .hex / .s19)'''#13#10 +
    '    end'#13#10 +
    '    object btnPick: TButton'#13#10 +
    '      Left = 484; Top = 10; Width = 90; Height = 28'#13#10 +
    '      Caption = ''Browse…'''#13#10 +
    '    end'#13#10 +
    '    object btnRun: TButton'#13#10 +
    '      Left = 586; Top = 10; Width = 100; Height = 28'#13#10 +
    '      Caption = ''Flash'''#13#10 +
    '    end'#13#10 +
    '    object pb: TProgressBar'#13#10 +
    '      Left = 12; Top = 48; Width = 674; Height = 18'#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET'#13#10 +
    '    Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12'#13#10 +
    '    Font.Name = ''Consolas'''#13#10 +
    '    Font.Style = []'#13#10 +
    '    ParentFont = False'#13#10 +
    '    ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection'#13#10 +
    '    Left = 88; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter'#13#10 +
    '    Left = 152; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object AuditLog: TOBDCodingAuditLog'#13#10 +
    '    Left = 216; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object UDSTransfer: TOBDUDSTransfer'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    AutoExecute = False'#13#10 +
    '    Left = 280; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object VoltageGate: TOBDVoltageGate'#13#10 +
    '    Left = 344; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object Pipeline: TOBDFlashPipeline'#13#10 +
    '    Protocol    = Protocol'#13#10 +
    '    AuditLog    = AuditLog'#13#10 +
    '    VoltageGate = VoltageGate'#13#10 +
    '    AutoExecute = False'#13#10 +
    '    Left = 408; Top = 96'#13#10 +
    '  end'#13#10 +
    '  object dlg: TOpenDialog'#13#10 +
    '    Filter = ''Firmware images (*.bin;*.hex;*.s19)|*.bin;*.hex;*.s19|All files (*.*)|*.*'''#13#10 +
    '    Left = 472; Top = 96'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := Expand(PAS_TPL, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := Expand(DFM_TPL, AContext);
end;

{ ---- 07 — Recorder / replayer workbench ------------------------------------- }

function GenerateRecorder(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10 +
    ''#13#10 +
    'interface'#13#10 +
    ''#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol,'#13#10 +
    '  OBD.Recorder, OBD.Replayer;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    Recorder:   TOBDRecorder;'#13#10 +
    '    Replayer:   TOBDReplayer;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    btnRecord:  TButton;'#13#10 +
    '    btnStop:    TButton;'#13#10 +
    '    btnReplay:  TButton;'#13#10 +
    '    edtFile:    TEdit;'#13#10 +
    '    memLog:     TMemo;'#13#10 +
    '    dlg:        TSaveDialog;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};'#13#10 +
    ''#13#10 +
    'implementation'#13#10 +
    ''#13#10 +
    '{$R *.dfm}'#13#10 +
    ''#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - recorder workbench'''#13#10 +
    '  ClientHeight = 480'#13#10 +
    '  ClientWidth = 640'#13#10 +
    '  Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop'#13#10 +
    '    Height = 56'#13#10 +
    '    BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12'#13#10 +
    '    Padding.Top = 14'#13#10 +
    '    Padding.Right = 12'#13#10 +
    '    Padding.Bottom = 12'#13#10 +
    '    object edtFile: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 360; Height = 24'#13#10 +
    '      TextHint = ''recording.obdlog'''#13#10 +
    '    end'#13#10 +
    '    object btnRecord: TButton'#13#10 +
    '      Left = 384; Top = 12; Width = 70; Height = 28'#13#10 +
    '      Caption = ''Record'''#13#10 +
    '    end'#13#10 +
    '    object btnStop: TButton'#13#10 +
    '      Left = 462; Top = 12; Width = 70; Height = 28'#13#10 +
    '      Caption = ''Stop'''#13#10 +
    '    end'#13#10 +
    '    object btnReplay: TButton'#13#10 +
    '      Left = 540; Top = 12; Width = 80; Height = 28'#13#10 +
    '      Caption = ''Replay'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET'#13#10 +
    '    Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12'#13#10 +
    '    Font.Name = ''Consolas'''#13#10 +
    '    Font.Style = []'#13#10 +
    '    ParentFont = False'#13#10 +
    '    ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection'#13#10 +
    '    Left = 88; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter'#13#10 +
    '    Left = 152; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Recorder: TOBDRecorder'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    Left = 216; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Replayer: TOBDReplayer'#13#10 +
    '    Left = 280; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object dlg: TSaveDialog'#13#10 +
    '    Filter = ''OBD logs (*.obdlog;*.obdlog.gz)|*.obdlog;*.obdlog.gz|All files (*.*)|*.*'''#13#10 +
    '    Left = 344; Top = 80'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := Expand(PAS_TPL, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := Expand(DFM_TPL, AContext);
end;

{ ---- registration ----------------------------------------------------------- }

procedure RegisterAll;
var
  S: TOBDStarter;
begin
  S.Id          := 'empty';
  S.Title       := 'Empty (connection chain only)';
  S.Description := 'A bare project with TOBDConnection, TOBDAdapter, TOBDProtocol ' +
                   'wired together. Start here if your scenario isn''t covered ' +
                   'by a more specific starter.';
  S.Category    := 'Foundation';
  S.Generate    := GenerateEmpty;
  TOBDStarterRegistry.Default.Register(S);

  S.Id          := 'dtc-reader';
  S.Title       := 'DTC reader / reset (ELM327)';
  S.Description := 'Read and clear stored DTCs over OBD-II Service 03 / 04. ' +
                   'Single form with two buttons and a list view.';
  S.Category    := 'Service-mode';
  S.Generate    := GenerateDTC;
  TOBDStarterRegistry.Default.Register(S);

  S.Id          := 'vin-reader';
  S.Title       := 'VIN reader';
  S.Description := 'Read the VIN over OBD-II Service 09 PID 02 (legacy) or ' +
                   'UDS DID 0xF190. Single button, single label.';
  S.Category    := 'Service-mode';
  S.Generate    := GenerateVIN;
  TOBDStarterRegistry.Default.Register(S);

  S.Id          := 'live-data';
  S.Title       := 'Live data dashboard';
  S.Description := 'Cycles a few PIDs (RPM, speed, coolant) at 250 ms and ' +
                   'displays the values in a string grid. Edit the PID array ' +
                   'in btnStartClick to add your own.';
  S.Category    := 'Service-mode';
  S.Generate    := GenerateLiveData;
  TOBDStarterRegistry.Default.Register(S);

  S.Id          := 'coding-session';
  S.Title       := 'Coding session (DIDs + security access)';
  S.Description := 'Drops TOBDSecurityAccess + TOBDDataIdentifierIO + ' +
                   'TOBDCodingAuditLog + TOBDCodingSession on a form with ' +
                   'a DID hex input, data input and read / write buttons.';
  S.Category    := 'Coding';
  S.Generate    := GenerateCoding;
  TOBDStarterRegistry.Default.Register(S);

  S.Id          := 'flash-session';
  S.Title       := 'Flash session (UDS pipeline)';
  S.Description := 'Drops the full TOBDFlashPipeline + TOBDVoltageGate + ' +
                   'TOBDUDSTransfer + TOBDCodingAuditLog stack on a form ' +
                   'with image picker, run button and progress bar. ' +
                   'AutoExecute = False by default — read docs/flashing-safety.md.';
  S.Category    := 'Flashing';
  S.Generate    := GenerateFlash;
  TOBDStarterRegistry.Default.Register(S);

  S.Id          := 'recorder-replayer';
  S.Title       := 'Recorder / replayer workbench';
  S.Description := 'Drops TOBDRecorder + TOBDReplayer on a form. Capture a ' +
                   'live session to disk, then replay the .obdlog through the ' +
                   'protocol event surface for offline analysis.';
  S.Category    := 'Tooling';
  S.Generate    := GenerateRecorder;
  TOBDStarterRegistry.Default.Register(S);
end;

initialization
  RegisterAll;

end.
