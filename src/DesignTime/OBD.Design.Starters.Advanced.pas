//------------------------------------------------------------------------------
//  OBD.Design.Starters.Advanced
//
//  Calibration / network / tooling / suite starters for the
//  Delphi-OBD wizard:
//
//    xcp-master         TOBDXCP master with transport choice.
//    ccp-master         TOBDCCP master.
//    isobus-implement   TOBDIsoBus implement node.
//    doip-client        TOBDDoIPClient with optional TLS / audit.
//    secoc-wrap         TOBDSecOCCodec wrap / unwrap harness.
//    protocol-mock      TOBDProtocolMock — drive tests from a
//                       recorded .obdlog instead of a live ECU.
//    full-suite         Multi-tab kitchen-sink of every Delphi-OBD
//                       module the host ticks (diagnostics +
//                       coding + flashing + tooling).
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Starters.Advanced;

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

{ ---- xcp-master ------------------------------------------------------------ }

function GenerateXCPMaster(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
var
  Transport, PasOut: string;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Calibration.XCP;'#13#10 +
    ''#13#10 +
    '// Transport: {TRANSPORT}. Wire the actual transport via'#13#10 +
    '// XCP.SetTransport(...) at runtime.'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    XCP:        TOBDXCP;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    btnConnect: TButton;'#13#10 +
    '    btnGetVer:  TButton;'#13#10 +
    '    memLog:     TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - XCP master'''#13#10 +
    '  ClientHeight = 420; ClientWidth = 600; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object btnConnect: TButton'#13#10 +
    '      Left = 12; Top = 12; Width = 90; Height = 28'#13#10 +
    '      Caption = ''Connect'''#13#10 +
    '    end'#13#10 +
    '    object btnGetVer: TButton'#13#10 +
    '      Left = 110; Top = 12; Width = 130; Height = 28'#13#10 +
    '      Caption = ''GET_VERSION'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object XCP: TOBDXCP'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
begin
  Transport := AContext.FirstChoice('transport', 'can');
  PasOut := StringReplace(PAS_BODY, '{TRANSPORT}',
    UpperCase(Transport), []);
  SetLength(Result, 3);
  Result[0] := MakeDprArtifact(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := PasShell(PasOut, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := ExpandTemplate(DFM_TPL, AContext);
end;

{ ---- ccp-master ------------------------------------------------------------ }

function GenerateCCPMaster(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Calibration.CCP;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    CCP:        TOBDCCP;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    btnConnect: TButton;'#13#10 +
    '    memLog:     TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - CCP master'''#13#10 +
    '  ClientHeight = 420; ClientWidth = 600; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object btnConnect: TButton'#13#10 +
    '      Left = 12; Top = 12; Width = 90; Height = 28'#13#10 +
    '      Caption = ''Connect'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object CCP: TOBDCCP'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
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

{ ---- isobus-implement ------------------------------------------------------ }

function GenerateIsoBus(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Speciality.IsoBus;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    IsoBus:    TOBDIsoBus;'#13#10 +
    '    pnl:       TPanel;'#13#10 +
    '    btnClaim:  TButton;'#13#10 +
    '    memLog:    TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - IsoBus implement'''#13#10 +
    '  ClientHeight = 420; ClientWidth = 600; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object btnClaim: TButton'#13#10 +
    '      Left = 12; Top = 12; Width = 130; Height = 28'#13#10 +
    '      Caption = ''Claim address'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object IsoBus: TOBDIsoBus'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
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

{ ---- doip-client ----------------------------------------------------------- }

function GenerateDoIPClient(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
var
  Comments: string;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Protocol.DoIP.Client;'#13#10 +
    ''#13#10 +
    '{COMMENTS}'#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    DoIP:       TOBDDoIPClient;'#13#10 +
    '    pnl:        TPanel;'#13#10 +
    '    edtVIN:     TEdit;'#13#10 +
    '    edtIP:      TEdit;'#13#10 +
    '    btnConnect: TButton;'#13#10 +
    '    memLog:     TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - DoIP client'''#13#10 +
    '  ClientHeight = 420; ClientWidth = 600; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtVIN: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 200; Height = 24'#13#10 +
    '      TextHint = ''Target VIN'''#13#10 +
    '    end'#13#10 +
    '    object edtIP: TEdit'#13#10 +
    '      Left = 224; Top = 14; Width = 160; Height = 24'#13#10 +
    '      TextHint = ''IP:port'''#13#10 +
    '    end'#13#10 +
    '    object btnConnect: TButton'#13#10 +
    '      Left = 400; Top = 12; Width = 110; Height = 28'#13#10 +
    '      Caption = ''Connect'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object DoIP: TOBDDoIPClient'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
    '  end'#13#10 +
    'end'#13#10;
var
  PasOut: string;
begin
  Comments := '';
  if AContext.HasChoice('extras', 'tls') then
    Comments := Comments +
      '// TLS extra selected: at runtime, set the DoIP transport to'#13#10 +
      '//   OBD.Protocol.DoIP.TLS.OpenSSL.TOBDDoIPOpenSSLTransport.'#13#10;
  if AContext.HasChoice('extras', 'audit-log') then
    Comments := Comments +
      '// Audit-log extra selected: bind a TOBDCodingAuditLog to the'#13#10 +
      '//   DoIP transport via the OnFrame / OnError events.'#13#10;
  PasOut := StringReplace(PAS_BODY, '{COMMENTS}', TrimRight(Comments), []);

  SetLength(Result, 3);
  Result[0] := MakeDprArtifact(AContext);
  Result[1].RelativePath := AContext.UnitName + '.pas';
  Result[1].Content      := PasShell(PasOut, AContext);
  Result[2].RelativePath := AContext.UnitName + '.dfm';
  Result[2].Content      := ExpandTemplate(DFM_TPL, AContext);
end;

{ ---- secoc-wrap ------------------------------------------------------------ }

function GenerateSecOC(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,'#13#10 +
    '  OBD.Protocol.SecOC, OBD.Protocol.SecOC.Keys;'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    SecOC:     TOBDSecOCCodec;'#13#10 +
    '    pnl:       TPanel;'#13#10 +
    '    edtDataID: TEdit;'#13#10 +
    '    edtPDU:    TEdit;'#13#10 +
    '    btnWrap:   TButton;'#13#10 +
    '    btnUnwrap: TButton;'#13#10 +
    '    memLog:    TMemo;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - SecOC wrap / unwrap'''#13#10 +
    '  ClientHeight = 460; ClientWidth = 700; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtDataID: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 100; Height = 24'#13#10 +
    '      TextHint = ''DataID hex'''#13#10 +
    '    end'#13#10 +
    '    object edtPDU: TEdit'#13#10 +
    '      Left = 124; Top = 14; Width = 360; Height = 24'#13#10 +
    '      TextHint = ''Original PDU hex'''#13#10 +
    '    end'#13#10 +
    '    object btnWrap: TButton'#13#10 +
    '      Left = 500; Top = 12; Width = 80; Height = 28'#13#10 +
    '      Caption = ''Wrap'''#13#10 +
    '    end'#13#10 +
    '    object btnUnwrap: TButton'#13#10 +
    '      Left = 588; Top = 12; Width = 90; Height = 28'#13#10 +
    '      Caption = ''Unwrap'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object SecOC: TOBDSecOCCodec'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
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

{ ---- protocol-mock --------------------------------------------------------- }

function GenerateProtocolMock(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_BODY =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs,'#13#10 +
    '  OBD.Recorder.ProtocolMock;'#13#10 +
    ''#13#10 +
    '// Drop-in TOBDProtocol replacement that drives events from a'#13#10 +
    '// captured .obdlog. Useful for replay-driven CI tests.'#13#10 +
    ''#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Mock:      TOBDProtocolMock;'#13#10 +
    '    pnl:       TPanel;'#13#10 +
    '    edtFile:   TEdit;'#13#10 +
    '    btnPick:   TButton;'#13#10 +
    '    btnRun:    TButton;'#13#10 +
    '    memLog:    TMemo;'#13#10 +
    '    dlg:       TOpenDialog;'#13#10 +
    '  end;'#13#10 +
    ''#13#10 +
    'var'#13#10 +
    '  {FORMVAR}: {FORM};';
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - protocol mock'''#13#10 +
    '  ClientHeight = 460; ClientWidth = 700; Position = poScreenCenter'#13#10 +
    '  object pnl: TPanel'#13#10 +
    '    Align = alTop; Height = 56; BevelOuter = bvNone'#13#10 +
    '    Padding.Left = 12; Padding.Top = 14; Padding.Right = 12; Padding.Bottom = 12'#13#10 +
    '    object edtFile: TEdit'#13#10 +
    '      Left = 12; Top = 14; Width = 460; Height = 24'#13#10 +
    '      TextHint = ''Recording (.obdlog or .obdlog.gz)'''#13#10 +
    '    end'#13#10 +
    '    object btnPick: TButton'#13#10 +
    '      Left = 484; Top = 12; Width = 90; Height = 28'#13#10 +
    '      Caption = ''Browse...'''#13#10 +
    '    end'#13#10 +
    '    object btnRun: TButton'#13#10 +
    '      Left = 586; Top = 12; Width = 90; Height = 28'#13#10 +
    '      Caption = ''Run'''#13#10 +
    '    end'#13#10 +
    '  end'#13#10 +
    '  object memLog: TMemo'#13#10 +
    '    Align = alClient'#13#10 +
    '    Font.Charset = DEFAULT_CHARSET; Font.Color = clWindowText'#13#10 +
    '    Font.Height = -12; Font.Name = ''Consolas''; Font.Style = []'#13#10 +
    '    ParentFont = False; ScrollBars = ssBoth'#13#10 +
    '  end'#13#10 +
    '  object Mock: TOBDProtocolMock'#13#10 +
    '    Left = 24; Top = 80'#13#10 +
    '  end'#13#10 +
    '  object dlg: TOpenDialog'#13#10 +
    '    Filter = ''OBD logs (*.obdlog;*.obdlog.gz)|*.obdlog;*.obdlog.gz|All files (*.*)|*.*'''#13#10 +
    '    Left = 88; Top = 80'#13#10 +
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

{ ---- full-suite (kitchen sink) --------------------------------------------- }

function GenerateFullSuite(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
var
  Uses_, TypeDecl, DfmComps, DfmTabs, PasOut, DfmOut: string;
  Pos_: Integer;
const
  PAS_TPL =
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,'#13#10 +
    '  Vcl.Grids, Vcl.Dialogs,'#13#10 +
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
    '  Caption = ''{PROJ} - full suite'''#13#10 +
    '  ClientHeight = 600; ClientWidth = 880; Position = poScreenCenter'#13#10 +
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

  procedure AddModule(const AOptionId, AUnit, AField, AType,
    ATabCaption: string);
  begin
    if not AContext.HasChoice('modules', AOptionId) then Exit;
    Uses_    := Uses_    + ', ' + AUnit;
    TypeDecl := TypeDecl + '    ' + AField + ': ' + AType + ';' + sLineBreak;
    DfmTabs  := DfmTabs  +
      '    object ts' + AField + ': TTabSheet'#13#10 +
      '      Caption = ''' + ATabCaption + ''''#13#10 +
      '    end'#13#10;
    DfmComps := DfmComps +
      '  object ' + AField + ': ' + AType + #13#10 +
      '    Protocol = Protocol'#13#10 +
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

  AddModule('live-data',    'OBD.Service.LiveData',           'LiveData',    'TOBDLiveData',         'Live data');
  AddModule('dtcs',         'OBD.Service.DTCs',               'DTCs',        'TOBDDTCs',             'DTCs');
  AddModule('vin',          'OBD.Service.VIN',                'VIN',         'TOBDVIN',              'VIN');
  AddModule('freeze-frame', 'OBD.Service.FreezeFrame',        'FreezeFrame', 'TOBDFreezeFrame',      'Freeze frame');
  AddModule('coding',       'OBD.Coding.DataIdentifierIO',    'DataIO',      'TOBDDataIdentifierIO', 'Coding');
  AddModule('flashing',     'OBD.Flash.Pipeline',             'Pipeline',    'TOBDFlashPipeline',    'Flashing');

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
  S.Id := 'xcp-master';
  S.Title := 'XCP master';
  S.Description :=
    'TOBDXCP master with a chosen transport hint. Wire the actual ' +
    'transport (CAN / Ethernet / USB) at runtime via SetTransport.';
  S.Category := 'Calibration';
  SetLength(S.OptionGroups, 1);
  S.OptionGroups[0].Id := 'transport';
  S.OptionGroups[0].Title := 'Transport';
  S.OptionGroups[0].Description :=
    'Pick the transport you intend to use; the wizard adds a ' +
    'comment to the .pas pointing at the matching code-side hookup.';
  S.OptionGroups[0].Kind := sokSingleSelect;
  SetLength(S.OptionGroups[0].Options, 3);
  S.OptionGroups[0].Options[0].Id := 'can';
  S.OptionGroups[0].Options[0].Caption := 'CAN';
  S.OptionGroups[0].Options[0].Selected := True;
  S.OptionGroups[0].Options[1].Id := 'eth';
  S.OptionGroups[0].Options[1].Caption := 'Ethernet (UDP / TCP)';
  S.OptionGroups[0].Options[2].Id := 'usb';
  S.OptionGroups[0].Options[2].Caption := 'USB';
  S.Generate := GenerateXCPMaster;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'ccp-master';
  S.Title := 'CCP master';
  S.Description :=
    'CCP (CAN Calibration Protocol) master, the older sibling of XCP. ' +
    'Drops TOBDCCP on the form with a Connect button and a log memo.';
  S.Category := 'Calibration';
  S.Generate := GenerateCCPMaster;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'isobus-implement';
  S.Title := 'IsoBus implement (ISO 11783)';
  S.Description :=
    'Drops TOBDIsoBus on a form for an ISO 11783 implement node. ' +
    'Includes a NAME-claim button to kick off address claim.';
  S.Category := 'Calibration';
  S.Generate := GenerateIsoBus;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'doip-client';
  S.Title := 'DoIP client (ISO 13400)';
  S.Description :=
    'Drops TOBDDoIPClient with target VIN + IP inputs and a Connect ' +
    'button. Optional extras add comments pointing at the OpenSSL TLS ' +
    'transport and the audit-log hookup.';
  S.Category := 'Network';
  SetLength(S.OptionGroups, 1);
  S.OptionGroups[0].Id := 'extras';
  S.OptionGroups[0].Title := 'Optional extras';
  S.OptionGroups[0].Description :=
    'Each ticked entry adds a comment in the .pas explaining where ' +
    'to wire the matching subsystem.';
  S.OptionGroups[0].Kind := sokMultiSelect;
  SetLength(S.OptionGroups[0].Options, 2);
  S.OptionGroups[0].Options[0].Id := 'tls';
  S.OptionGroups[0].Options[0].Caption := 'TLS transport (OpenSSL)';
  S.OptionGroups[0].Options[1].Id := 'audit-log';
  S.OptionGroups[0].Options[1].Caption := 'Audit-log integration';
  S.Generate := GenerateDoIPClient;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'secoc-wrap';
  S.Title := 'SecOC wrap / unwrap';
  S.Description :=
    'TOBDSecOCCodec on a form with DataID + payload inputs and ' +
    'Wrap / Unwrap buttons. Useful as a key-store sanity test.';
  S.Category := 'Network';
  S.Generate := GenerateSecOC;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'protocol-mock';
  S.Title := 'Protocol mock from recording';
  S.Description :=
    'Drives integration tests against a recorded .obdlog instead of a ' +
    'live ECU. Drops TOBDProtocolMock on the form with a recording ' +
    'picker and a Run button.';
  S.Category := 'Tooling';
  S.Generate := GenerateProtocolMock;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'full-suite';
  S.Title := 'Full Delphi-OBD suite (kitchen sink)';
  S.Description :=
    'Multi-tab project that wires the host-selected modules together: ' +
    'live data, DTCs, VIN, freeze-frame, coding, flashing. Pick what ' +
    'you want, the wizard adds the matching tab and component.';
  S.Category := 'Suite';
  SetLength(S.OptionGroups, 1);
  S.OptionGroups[0].Id := 'modules';
  S.OptionGroups[0].Title := 'Modules';
  S.OptionGroups[0].Description :=
    'Each ticked module adds a TPageControl tab and the matching ' +
    'TOBD* component, all bound to the shared TOBDProtocol.';
  S.OptionGroups[0].Kind := sokMultiSelect;
  SetLength(S.OptionGroups[0].Options, 6);
  S.OptionGroups[0].Options[0].Id := 'live-data';
  S.OptionGroups[0].Options[0].Caption := 'Live data';
  S.OptionGroups[0].Options[0].Selected := True;
  S.OptionGroups[0].Options[1].Id := 'dtcs';
  S.OptionGroups[0].Options[1].Caption := 'DTCs';
  S.OptionGroups[0].Options[1].Selected := True;
  S.OptionGroups[0].Options[2].Id := 'vin';
  S.OptionGroups[0].Options[2].Caption := 'VIN';
  S.OptionGroups[0].Options[2].Selected := True;
  S.OptionGroups[0].Options[3].Id := 'freeze-frame';
  S.OptionGroups[0].Options[3].Caption := 'Freeze frame';
  S.OptionGroups[0].Options[3].Selected := True;
  S.OptionGroups[0].Options[4].Id := 'coding';
  S.OptionGroups[0].Options[4].Caption := 'Coding (DataIdentifierIO)';
  S.OptionGroups[0].Options[4].Selected := True;
  S.OptionGroups[0].Options[5].Id := 'flashing';
  S.OptionGroups[0].Options[5].Caption := 'Flashing (TOBDFlashPipeline)';
  S.OptionGroups[0].Options[5].Selected := False;
  S.Generate := GenerateFullSuite;
  TOBDStarterRegistry.Default.Register(S);
end;

initialization
  RegisterAll;

end.
