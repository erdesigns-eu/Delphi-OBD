//------------------------------------------------------------------------------
//  OBD.Design.Starters.Coding
//
//  Coding starters for the Delphi-OBD wizard:
//
//    security-access    Standalone seed/key harness.
//    routine-control    UDS RoutineControl runner.
//    uds-write-memory   UDS WriteMemoryByAddress.
//    kwp-write-id       KWP2000 WriteIdentifier (5-baud / fast).
//    oem-component-protection
//                       Drops the host-selected subset of VAG /
//                       BMW / Mercedes / Stellantis component-
//                       protection components into a single
//                       TPageControl-driven form.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Starters.Coding;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Design.Starters;

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

{ ---- security-access ------------------------------------------------------- }

function GenerateSecurityAccess(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Coding.SecurityAccess;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection:     TOBDConnection;'#13#10 +
    '    Adapter:        TOBDAdapter;'#13#10 +
    '    Protocol:       TOBDProtocol;'#13#10 +
    '    SecurityAccess: TOBDSecurityAccess;'#13#10 +
    '    pnl:            TPanel;'#13#10 +
    '    edtLevel:       TEdit;'#13#10 +
    '    btnUnlock:      TButton;'#13#10 +
    '    memLog:         TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - security access'''#13#10 +
    '  ClientHeight = 420; ClientWidth = 600; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtLevel: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 80; Height = 24'#13#10 +
    '      TextHint = ''Level (e.g. 01)'''#13#10 +
    '      Text = ''01'''#13#10 +
    '    end'#13#10 +
    '    object btnUnlock: TButton'#13#10 +
    '      Left = 100; Top = 12; Width = 110; Height = 28'#13#10 +
    '      Caption = ''Unlock'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection; Left = 88; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter; Left = 152; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object SecurityAccess: TOBDSecurityAccess'#13#10 +
    '    Protocol = Protocol; Left = 216; Top = 80'#13#10 +
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

{ ---- routine-control ------------------------------------------------------- }

function GenerateRoutineControl(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.Coding.RoutineControl;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    Routine:    TOBDRoutineControl;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    edtRID:     TEdit;'#13#10 +
    '    edtParams:  TEdit;'#13#10 +
    '    btnStart:   TButton;'#13#10 +
    '    btnStop:    TButton;'#13#10 +
    '    btnResult:  TButton;'#13#10 +
    '    memLog:     TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - routine control'''#13#10 +
    '  ClientHeight = 460; ClientWidth = 700; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtRID: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 100; Height = 24'#13#10 +
    '      TextHint = ''RID hex'''#13#10 +
    '    end'#13#10 +
    '    object edtParams: TEdit'#13#10 +
    '      Left = 124; Top = 14; Width = 220; Height = 24'#13#10 +
    '      TextHint = ''Parameters hex (optional)'''#13#10 +
    '    end'#13#10 +
    '    object btnStart: TButton'#13#10 +
    '      Left = 360; Top = 12; Width = 80; Height = 28'#13#10 +
    '      Caption = ''Start'''#13#10 +
    '    end'#13#10 +
    '    object btnStop: TButton'#13#10 +
    '      Left = 448; Top = 12; Width = 80; Height = 28'#13#10 +
    '      Caption = ''Stop'''#13#10 +
    '    end'#13#10 +
    '    object btnResult: TButton'#13#10 +
    '      Left = 536; Top = 12; Width = 110; Height = 28'#13#10 +
    '      Caption = ''Get result'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection; Left = 88; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter; Left = 152; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Routine: TOBDRoutineControl'#13#10 +
    '    Protocol = Protocol; Left = 216; Top = 80'#13#10 +
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

{ ---- uds-write-memory ------------------------------------------------------ }

function GenerateUDSWriteMemory(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.UDS.WriteMemory;'#13#10 +
    ''#13#10 +
    '// SAFETY: WriteMemory is destructive. AutoExecute defaults False;'#13#10 +
    '// keep it that way until you have wired OnConfirmExecute.'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection:  TOBDConnection;'#13#10 +
    '    Adapter:     TOBDAdapter;'#13#10 +
    '    Protocol:    TOBDProtocol;'#13#10 +
    '    WriteMemory: TOBDUDSWriteMemory;'#13#10 +
    '    pnl:         TPanel;'#13#10 +
    '    edtAddr:     TEdit;'#13#10 +
    '    edtData:     TEdit;'#13#10 +
    '    btnWrite:    TButton;'#13#10 +
    '    memLog:      TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - UDS WriteMemory'''#13#10 +
    '  ClientHeight = 460; ClientWidth = 700; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtAddr: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 140; Height = 24'#13#10 +
    '      TextHint = ''Address hex (e.g. 8000_0000)'''#13#10 +
    '    end'#13#10 +
    '    object edtData: TEdit'#13#10 +
    '      Left = 164; Top = 14; Width = 360; Height = 24'#13#10 +
    '      TextHint = ''Data bytes hex'''#13#10 +
    '    end'#13#10 +
    '    object btnWrite: TButton'#13#10 +
    '      Left = 540; Top = 12; Width = 110; Height = 28'#13#10 +
    '      Caption = ''Write'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection; Left = 88; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter; Left = 152; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object WriteMemory: TOBDUDSWriteMemory'#13#10 +
    '    Protocol = Protocol; AutoExecute = False'#13#10 +
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

{ ---- kwp-write-id ---------------------------------------------------------- }

function GenerateKWPWriteID(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol, OBD.KWP.WriteID;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Connection: TOBDConnection;'#13#10 +
    '    Adapter:    TOBDAdapter;'#13#10 +
    '    Protocol:   TOBDProtocol;'#13#10 +
    '    WriteID:    TOBDKWPWriteID;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    edtID:      TEdit;'#13#10 +
    '    edtData:    TEdit;'#13#10 +
    '    btnWrite:   TButton;'#13#10 +
    '    memLog:     TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - KWP WriteID'''#13#10 +
    '  ClientHeight = 460; ClientWidth = 700; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtID: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 100; Height = 24'#13#10 +
    '      TextHint = ''ID hex'''#13#10 +
    '    end'#13#10 +
    '    object edtData: TEdit'#13#10 +
    '      Left = 124; Top = 14; Width = 360; Height = 24'#13#10 +
    '      TextHint = ''Data bytes hex'''#13#10 +
    '    end'#13#10 +
    '    object btnWrite: TButton'#13#10 +
    '      Left = 500; Top = 12; Width = 110; Height = 28'#13#10 +
    '      Caption = ''Write'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection; Left = 88; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter; Left = 152; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object WriteID: TOBDKWPWriteID'#13#10 +
    '    Protocol = Protocol; AutoExecute = False'#13#10 +
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

{ ---- oem-component-protection ---------------------------------------------- }

function GenerateOEMCP(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,'#13#10 +
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
    '  Caption = ''{PROJ} - component protection'''#13#10 +
    '  ClientHeight = 480; ClientWidth = 720; Position = poScreenCenter'#13#10 +
    '  object pages: TPageControl'#13#10 +
    '    Align = alClient'#13#10 +
    '{DFM_TABS}'#13#10 +
    '  end'#13#10 +
    '  object Connection: TOBDConnection'#13#10 +
    '    Left = 24; Top = 24'#13#10 +
    '  end'#13#10 +
    '  object Adapter: TOBDAdapter'#13#10 +
    '    Connection = Connection; Left = 88; Top = 24'#13#10 +
    '  end'#13#10 +
    '  object Protocol: TOBDProtocol'#13#10 +
    '    Adapter = Adapter; Left = 152; Top = 24'#13#10 +
    '  end'#13#10 +
    '{DFM_COMPONENTS}'#13#10 +
    'end'#13#10;
var
  Uses_, TypeDecl, DfmComps, DfmTabs: string;
  Pos_: Integer;
  PasOut, DfmOut: string;

  procedure AddOEM(const AOptionId, AUnit, AField, AType,
    ATabCaption: string);
  begin
    if not AContext.HasChoice('oems', AOptionId) then Exit;
    Uses_    := Uses_    + ', ' + AUnit;
    TypeDecl := TypeDecl + '    ' + AField + ': ' + AType + ';' + sLineBreak;
    DfmTabs  := DfmTabs  +
      '    object ts' + AField + ': TTabSheet'#13#10 +
      '      Caption = ''' + ATabCaption + ''''#13#10 +
      '    end'#13#10;
    DfmComps := DfmComps +
      '  object ' + AField + ': ' + AType + #13#10 +
      '    Protocol = Protocol; AutoExecute = False'#13#10 +
      '    Left = ' + IntToStr(216 + Pos_ * 64) + '; Top = 24'#13#10 +
      '  end'#13#10;
    Inc(Pos_);
  end;

begin
  Uses_ := '';
  TypeDecl := '';
  DfmComps := '';
  DfmTabs := '';
  Pos_ := 0;

  AddOEM('vag',        'OBD.OEM.ComponentProtection.VAG',        'CP_VAG',        'TOBDComponentProtectionVAG',        'Volkswagen group');
  AddOEM('bmw',        'OBD.OEM.ComponentProtection.BMW',        'CP_BMW',        'TOBDComponentProtectionBMW',        'BMW');
  AddOEM('mercedes',   'OBD.OEM.ComponentProtection.Mercedes',   'CP_Mercedes',   'TOBDComponentProtectionMercedes',   'Mercedes-Benz');
  AddOEM('stellantis', 'OBD.OEM.ComponentProtection.Stellantis', 'CP_Stellantis', 'TOBDComponentProtectionStellantis', 'Stellantis');

  PasOut := StringReplace(PAS_TPL, '{USES_EXTRA}', Uses_, []);
  PasOut := StringReplace(PasOut,  '{TYPE_EXTRA}', TrimRight(TypeDecl), []);
  DfmOut := StringReplace(DFM_TPL, '{DFM_TABS}',       TrimRight(DfmTabs),  []);
  DfmOut := StringReplace(DfmOut,  '{DFM_COMPONENTS}', TrimRight(DfmComps), []);

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
  S.Id := 'security-access';
  S.Title := 'Security access harness';
  S.Description :=
    'Standalone seed-and-key harness. Drops TOBDSecurityAccess on a form ' +
    'with a Level input and an Unlock button. Wire OnComputeKey on the ' +
    'component for your seed-to-key algorithm.';
  S.Category := 'Coding';
  S.Generate := GenerateSecurityAccess;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'routine-control';
  S.Title := 'Routine control runner';
  S.Description :=
    'UDS RoutineControl runner. Drops TOBDRoutineControl with RID + ' +
    'parameters inputs and Start / Stop / Get-result buttons.';
  S.Category := 'Coding';
  S.Generate := GenerateRoutineControl;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'uds-write-memory';
  S.Title := 'UDS WriteMemoryByAddress';
  S.Description :=
    'Drops TOBDUDSWriteMemory on a form with address + data inputs and ' +
    'a Write button. AutoExecute = False; wire OnConfirmExecute before ' +
    'going live — WriteMemory is destructive.';
  S.Category := 'Coding';
  S.Generate := GenerateUDSWriteMemory;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'kwp-write-id';
  S.Title := 'KWP2000 WriteIdentifier';
  S.Description :=
    'Drops TOBDKWPWriteID with ID + data inputs and a Write button. ' +
    'AutoExecute = False; KWP WriteID can change identifying blocks ' +
    '(odometer, immobilizer) — wire OnConfirmExecute.';
  S.Category := 'Coding';
  S.Generate := GenerateKWPWriteID;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'oem-component-protection';
  S.Title := 'OEM component protection (multi-OEM)';
  S.Description :=
    'Drops the host-selected subset of VAG / BMW / Mercedes / Stellantis ' +
    'component-protection components onto a TPageControl, one tab each.';
  S.Category := 'Coding';
  SetLength(S.OptionGroups, 1);
  S.OptionGroups[0].Id := 'oems';
  S.OptionGroups[0].Title := 'Manufacturer groups';
  S.OptionGroups[0].Description :=
    'Each ticked entry adds a TPageControl tab and the matching ' +
    'TOBDComponentProtection* component (AutoExecute = False).';
  S.OptionGroups[0].Kind := sokMultiSelect;
  SetLength(S.OptionGroups[0].Options, 4);
  S.OptionGroups[0].Options[0].Id := 'vag';
  S.OptionGroups[0].Options[0].Caption := 'Volkswagen / Audi / Skoda / SEAT';
  S.OptionGroups[0].Options[0].Selected := True;
  S.OptionGroups[0].Options[1].Id := 'bmw';
  S.OptionGroups[0].Options[1].Caption := 'BMW';
  S.OptionGroups[0].Options[1].Selected := True;
  S.OptionGroups[0].Options[2].Id := 'mercedes';
  S.OptionGroups[0].Options[2].Caption := 'Mercedes-Benz';
  S.OptionGroups[0].Options[2].Selected := True;
  S.OptionGroups[0].Options[3].Id := 'stellantis';
  S.OptionGroups[0].Options[3].Caption := 'Stellantis';
  S.OptionGroups[0].Options[3].Selected := True;
  S.Generate := GenerateOEMCP;
  TOBDStarterRegistry.Default.Register(S);
end;

initialization
  RegisterAll;

end.
