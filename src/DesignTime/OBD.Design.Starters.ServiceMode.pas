//------------------------------------------------------------------------------
//  OBD.Design.Starters.ServiceMode
//
//  Service-mode starters for the Delphi-OBD wizard:
//
//    freeze-frame      Freeze-frame inspector for one DTC.
//    onboard-monitor   On-board monitor / readiness flags.
//    actuator-test     Mode 08 actuator drive.
//    full-diagnostics  Multi-tab dashboard with the host's
//                      chosen subset of service-mode tabs.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Starters.ServiceMode;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Design.Starters;

{ ---- helpers --------------------------------------------------------------- }

function PasShell(const ABody: string;
  const AContext: TOBDStarterContext): string;
const
  TPL =
    'unit {UNIT};'#13#10 +
    ''#13#10 +
    'interface'#13#10 +
    ''#13#10 +
    '{BODY}'#13#10 +
    ''#13#10 +
    'implementation'#13#10 +
    ''#13#10 +
    '{$R *.dfm}'#13#10 +
    ''#13#10 +
    'end.'#13#10;
begin
  Result := ExpandTemplate(StringReplace(TPL, '{BODY}', ABody, []), AContext);
end;

{ ---- freeze-frame ---------------------------------------------------------- }

function GenerateFreezeFrame(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Service.FreezeFrame;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection:  TOBDConnection;'#13#10 +
    '    Adapter:     TOBDAdapter;'#13#10 +
    '    Protocol:    TOBDProtocol;'#13#10 +
    '    FreezeFrame: TOBDFreezeFrame;'#13#10 +
    '    pnl:         TPanel;'#13#10 +
    '    edtDTC:      TEdit;'#13#10 +
    '    btnRead:     TButton;'#13#10 +
    '    memOut:      TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - freeze frame'''#13#10 +
    '  ClientHeight = 420'#13#10 +
    '  ClientWidth = 600'#13#10 +
    '  Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop'#13#10 +
    '    Height = 56'#13#10 +
    '    BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtDTC: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 180; Height = 24'#13#10 +
    '      TextHint = ''DTC (e.g. P0420)'''#13#10 +
    '    end'#13#10 +
    '    object btnRead: TButton'#13#10 +
    '      Left = 200; Top = 12; Width = 130; Height = 28'#13#10 +
    '      Caption = ''Read freeze frame'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memOut: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
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
    '  object FreezeFrame: TOBDFreezeFrame'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    Left = 216; Top = 80'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDprArtifact(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := PasShell(PAS_BODY, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := ExpandTemplate(DFM_TPL, AContext);
end;

{ ---- onboard-monitor ------------------------------------------------------- }

function GenerateOnBoardMonitor(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Service.OnBoardMonitor;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    Monitor:    TOBDOnBoardMonitor;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    btnRead:    TButton;'#13#10 +
    '    memOut:     TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - onboard monitor'''#13#10 +
    '  ClientHeight = 420'#13#10 +
    '  ClientWidth = 600'#13#10 +
    '  Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object btnRead: TButton'#13#10 +
    '      Left = 12; Top = 12; Width = 180; Height = 28'#13#10 +
    '      Caption = ''Read readiness flags'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memOut: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
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
    '  object Monitor: TOBDOnBoardMonitor'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    Left = 216; Top = 80'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDprArtifact(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := PasShell(PAS_BODY, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := ExpandTemplate(DFM_TPL, AContext);
end;

{ ---- actuator-test --------------------------------------------------------- }

function GenerateActuator(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Service.Actuator;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    Actuator:   TOBDActuator;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    edtTID:     TEdit;'#13#10 +
    '    edtArgs:    TEdit;'#13#10 +
    '    btnRun:     TButton;'#13#10 +
    '    memOut:     TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - actuator test'''#13#10 +
    '  ClientHeight = 420; ClientWidth = 600; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtTID: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 100; Height = 24'#13#10 +
    '      TextHint = ''TID hex'''#13#10 +
    '    end'#13#10 +
    '    object edtArgs: TEdit'#13#10 +
    '      Left = 124; Top = 14; Width = 220; Height = 24'#13#10 +
    '      TextHint = ''Arguments hex (optional)'''#13#10 +
    '    end'#13#10 +
    '    object btnRun: TButton'#13#10 +
    '      Left = 360; Top = 12; Width = 120; Height = 28'#13#10 +
    '      Caption = ''Run actuator'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memOut: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
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
    '  object Actuator: TOBDActuator'#13#10 +
    '    Protocol = Protocol'#13#10 +
    '    Left = 216; Top = 80'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  SetLength(Result, 3);
  Result[0] := MakeDprArtifact(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := PasShell(PAS_BODY, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := ExpandTemplate(DFM_TPL, AContext);
end;

{ ---- full-diagnostics (multi-tab) ------------------------------------------ }

function GenerateFullDiagnostics(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
var
  Uses_, TypeDecl, DfmComponents, DfmTabs: string;
  PasOut, DfmOut: string;
const
  PAS_TPL =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,'#13#10 +
    '  Vcl.Grids,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol{USES_EXTRA};'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    pages:      TPageControl;'#13#10 +
    '{TYPE_EXTRA}'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - diagnostics suite'''#13#10 +
    '  ClientHeight = 540; ClientWidth = 760; Position = poScreenCenter'#13#10 +
    '  object pages: TPageControl'#13#10 +
    '    Align = alClient'#13#10 +
    '{DFM_TABS}'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 24'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection'#13#10 +
    '    Left = 88; Top = 24'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter'#13#10 +
    '    Left = 152; Top = 24'#13#10 +
    '  end'#13#10 +
    '{DFM_COMPONENTS}'#13#10 +
    'end'#13#10;

  procedure AddService(const AOptionId, APasUnit, APasField, APasType,
    ATabCaption, ADfmComp: string; var APos: Integer);
  begin
    if not AContext.HasChoice('services', AOptionId) then Exit;
    Uses_         := Uses_         + ', ' + APasUnit;
    TypeDecl      := TypeDecl      + '    ' + APasField + ': ' + APasType + ';' + sLineBreak;
    DfmTabs       := DfmTabs       +
      '    object ts' + APasField + ': TTabSheet'#13#10 +
      '      Caption = ''' + ATabCaption + ''''#13#10 +
      '    end'#13#10;
    DfmComponents := DfmComponents +
      '  object ' + APasField + ': ' + APasType + #13#10 +
      '    Protocol = Protocol'#13#10 +
      '    Left = ' + IntToStr(216 + APos * 64) + '; Top = 24'#13#10 +
      '  end'#13#10;
    Inc(APos);
  end;

var
  Pos_: Integer;
begin
  Uses_ := '';
  TypeDecl := '';
  DfmTabs := '';
  DfmComponents := '';
  Pos_ := 0;

  AddService('live-data',    'OBD.Service.LiveData',        'LiveData',    'TOBDLiveData',       'Live data',    '', Pos_);
  AddService('dtcs',         'OBD.Service.DTCs',            'DTCs',        'TOBDDTCs',           'DTCs',         '', Pos_);
  AddService('vin',          'OBD.Service.VIN',             'VIN',         'TOBDVIN',            'VIN',          '', Pos_);
  AddService('freeze-frame', 'OBD.Service.FreezeFrame',     'FreezeFrame', 'TOBDFreezeFrame',    'Freeze frame', '', Pos_);
  AddService('monitor',      'OBD.Service.OnBoardMonitor',  'Monitor',     'TOBDOnBoardMonitor', 'Monitors',     '', Pos_);
  AddService('actuator',     'OBD.Service.Actuator',        'Actuator',    'TOBDActuator',       'Actuator',     '', Pos_);

  PasOut := StringReplace(PAS_TPL, '{USES_EXTRA}',  Uses_,    []);
  PasOut := StringReplace(PasOut,  '{TYPE_EXTRA}',  TrimRight(TypeDecl), []);

  DfmOut := StringReplace(DFM_TPL, '{DFM_TABS}',       TrimRight(DfmTabs),       []);
  DfmOut := StringReplace(DfmOut,  '{DFM_COMPONENTS}', TrimRight(DfmComponents), []);

  SetLength(Result, 3);
  Result[0] := MakeDprArtifact(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := PasShell(PasOut, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := ExpandTemplate(DfmOut, AContext);
end;

{ ---- registration ---------------------------------------------------------- }

procedure RegisterAll;
var
  S: TOBDStarter;
begin
  S := Default(TOBDStarter);
  S.Id := 'freeze-frame';
  S.Title := 'Freeze frame inspector';
  S.Description :=
    'Reads the freeze-frame snapshot stored against a chosen DTC. ' +
    'Drops Connection / Adapter / Protocol / TOBDFreezeFrame on a form ' +
    'with a DTC input, a Read button and a result memo.';
  S.Category := 'Service-mode';
  S.Generate := GenerateFreezeFrame;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'onboard-monitor';
  S.Title := 'Onboard monitor / readiness flags';
  S.Description :=
    'Reads the on-board monitor / readiness flags. Useful for emissions ' +
    'pre-checks. Drops TOBDOnBoardMonitor on a form with a Read button ' +
    'and a result memo.';
  S.Category := 'Service-mode';
  S.Generate := GenerateOnBoardMonitor;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'actuator-test';
  S.Title := 'Actuator test (Mode 08)';
  S.Description :=
    'Drives a Mode 08 actuator routine. Drops TOBDActuator with TID and ' +
    'Arguments inputs and a Run button. Note: real actuator tests can ' +
    'move physical components — use with care.';
  S.Category := 'Service-mode';
  S.Generate := GenerateActuator;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'full-diagnostics';
  S.Title := 'Full diagnostics suite (multi-tab)';
  S.Description :=
    'Multi-tab diagnostics dashboard. Pick the service-mode tabs you ' +
    'want and the wizard wires the matching components into a TPageControl.';
  S.Category := 'Service-mode';
  SetLength(S.OptionGroups, 1);
  S.OptionGroups[0].Id := 'services';
  S.OptionGroups[0].Title := 'Service-mode tabs';
  S.OptionGroups[0].Description :=
    'Each ticked entry adds a TPageControl tab and the matching ' +
    'TOBD* component, all bound to the shared TOBDProtocol.';
  S.OptionGroups[0].Kind := sokMultiSelect;
  SetLength(S.OptionGroups[0].Options, 6);
  S.OptionGroups[0].Options[0].Id := 'live-data';
  S.OptionGroups[0].Options[0].Caption := 'Live data (TOBDLiveData)';
  S.OptionGroups[0].Options[0].Selected := True;
  S.OptionGroups[0].Options[1].Id := 'dtcs';
  S.OptionGroups[0].Options[1].Caption := 'DTC reader / clear (TOBDDTCs)';
  S.OptionGroups[0].Options[1].Selected := True;
  S.OptionGroups[0].Options[2].Id := 'vin';
  S.OptionGroups[0].Options[2].Caption := 'VIN reader (TOBDVIN)';
  S.OptionGroups[0].Options[2].Selected := True;
  S.OptionGroups[0].Options[3].Id := 'freeze-frame';
  S.OptionGroups[0].Options[3].Caption := 'Freeze-frame (TOBDFreezeFrame)';
  S.OptionGroups[0].Options[3].Selected := True;
  S.OptionGroups[0].Options[4].Id := 'monitor';
  S.OptionGroups[0].Options[4].Caption := 'Onboard monitor (TOBDOnBoardMonitor)';
  S.OptionGroups[0].Options[4].Selected := True;
  S.OptionGroups[0].Options[5].Id := 'actuator';
  S.OptionGroups[0].Options[5].Caption := 'Actuator test (TOBDActuator)';
  S.OptionGroups[0].Options[5].Selected := False;
  S.Generate := GenerateFullDiagnostics;
  TOBDStarterRegistry.Default.Register(S);
end;

initialization
  RegisterAll;

end.
