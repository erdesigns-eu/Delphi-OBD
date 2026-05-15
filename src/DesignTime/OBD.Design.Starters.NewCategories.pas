//------------------------------------------------------------------------------
//  OBD.Design.Starters.NewCategories
//
//  Adds minimal starters for the four palette tabs that didn't
//  have any in the original 26-starter set:
//
//    Radio       - single-vendor calculator form
//    EEPROM      - .bin extractor form
//    Catalogs    - catalogue-manager data module
//    Sessions    - KWP1281 session form
//
//  Each one is intentionally small (one form + a couple of
//  components) so users see the canonical wiring for the
//  category and can extend from there. The per-category IDE
//  wizards filter to these categories.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT - see LICENSE
//------------------------------------------------------------------------------

unit OBD.Design.Starters.NewCategories;

interface

implementation

uses
  System.SysUtils,
  System.Classes,
  OBD.Design.Starters;

function Expand(const ATemplate: string;
  const AContext: TOBDStarterContext): string; inline;
begin
  Result := ExpandTemplate(ATemplate, AContext);
end;

function MakeDpr(const AContext: TOBDStarterContext): TOBDStarterArtifact; inline;
begin
  Result := MakeDprArtifact(AContext);
end;

{ ---- Radio ------------------------------------------------------------------ }

function GenerateRadioForm(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,'#13#10 +
    '  OBD.RadioCode.Types, OBD.RadioCode,'#13#10 +
    '  OBD.RadioCode.FrenchItalian;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Calc:        TOBDRadioCodePeugeot;'#13#10 +
    '    edtSerial:   TEdit;'#13#10 +
    '    btnGo:       TButton;'#13#10 +
    '    lblResult:   TLabel;'#13#10 +
    '    procedure btnGoClick(Sender: TObject);'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'procedure {FORM}.btnGoClick(Sender: TObject);'#13#10 +
    'var R: TOBDRadioCodeResult;'#13#10 +
    'begin'#13#10 +
    '  Calc.Input := edtSerial.Text;'#13#10 +
    '  R := Calc.Calculate;'#13#10 +
    '  if R.Success then'#13#10 +
    '    lblResult.Caption := ''Code: '' + R.Code'#13#10 +
    '  else'#13#10 +
    '    lblResult.Caption := R.Message;'#13#10 +
    'end;'#13#10#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - Radio code'''#13#10 +
    '  ClientHeight = 200  ClientWidth = 360  Position = poScreenCenter'#13#10 +
    '  object Calc: TOBDRadioCodePeugeot Left = 16 Top = 16 end'#13#10 +
    '  object edtSerial: TEdit Left = 80 Top = 24 Width = 250 Text = '''' end'#13#10 +
    '  object btnGo: TButton Left = 80 Top = 64 Width = 250 Caption = ''Calculate'' OnClick = btnGoClick end'#13#10 +
    '  object lblResult: TLabel Left = 80 Top = 104 Width = 250 Caption = ''(result here)'' end'#13#10 +
    'end'#13#10;
var Pas, Dfm: TOBDStarterArtifact;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Pas := Default(TOBDStarterArtifact);
  Pas.RelativePath := AContext.UnitName + '.pas';
  Pas.Content := Expand(PAS_TPL, AContext);
  Result[1] := Pas;
  Dfm := Default(TOBDStarterArtifact);
  Dfm.RelativePath := AContext.UnitName + '.dfm';
  Dfm.Content := Expand(DFM_TPL, AContext);
  Result[2] := Dfm;
end;

{ ---- EEPROM ----------------------------------------------------------------- }

function GenerateEEPROMForm(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtDlgs, Vcl.Dialogs,'#13#10 +
    '  OBD.RadioCode.EEPROM;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Extractor:  TOBDRadioCodeEEPROM_VolvoHU;'#13#10 +
    '    OpenDlg:    TOpenDialog;'#13#10 +
    '    btnLoad:    TButton;'#13#10 +
    '    lblResult:  TLabel;'#13#10 +
    '    procedure btnLoadClick(Sender: TObject);'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'procedure {FORM}.btnLoadClick(Sender: TObject);'#13#10 +
    'var R: TOBDRadioCodeEEPROMResult;'#13#10 +
    'begin'#13#10 +
    '  if not OpenDlg.Execute then Exit;'#13#10 +
    '  Extractor.DumpFile := OpenDlg.FileName;'#13#10 +
    '  R := Extractor.Extract;'#13#10 +
    '  if R.Success then'#13#10 +
    '    lblResult.Caption := ''Code: '' + R.Code + ''   raw='' + R.RawHex'#13#10 +
    '  else'#13#10 +
    '    lblResult.Caption := R.Message;'#13#10 +
    'end;'#13#10#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - EEPROM extractor'''#13#10 +
    '  ClientHeight = 180  ClientWidth = 420  Position = poScreenCenter'#13#10 +
    '  object Extractor: TOBDRadioCodeEEPROM_VolvoHU Left = 16 Top = 16 end'#13#10 +
    '  object OpenDlg: TOpenDialog Filter = ''EEPROM dump|*.bin;*.eep|All|*.*'' Left = 64 Top = 16 end'#13#10 +
    '  object btnLoad: TButton Left = 80 Top = 24 Width = 320 Caption = ''Load .bin and extract'' OnClick = btnLoadClick end'#13#10 +
    '  object lblResult: TLabel Left = 80 Top = 64 Width = 320 Caption = ''(result here)'' end'#13#10 +
    'end'#13#10;
var Pas, Dfm: TOBDStarterArtifact;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Pas := Default(TOBDStarterArtifact);
  Pas.RelativePath := AContext.UnitName + '.pas';
  Pas.Content := Expand(PAS_TPL, AContext);
  Result[1] := Pas;
  Dfm := Default(TOBDStarterArtifact);
  Dfm.RelativePath := AContext.UnitName + '.dfm';
  Dfm.Content := Expand(DFM_TPL, AContext);
  Result[2] := Dfm;
end;

{ ---- Catalogs --------------------------------------------------------------- }

function GenerateCatalogsDM(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  OBD.Service.VINDecoder.Catalog.Component,'#13#10 +
    '  OBD.Service.DriveCycle.Catalog.Component,'#13#10 +
    '  OBD.Service.EVBattery.Catalog.Component;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TDataModule)'#13#10 +
    '    VINCatalog:        TOBDVINCatalog;'#13#10 +
    '    DriveCycleCatalog: TOBDDriveCycleCatalogComp;'#13#10 +
    '    EVCatalog:         TOBDEVBatteryCatalogComp;'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  OldCreateOrder = False'#13#10 +
    '  Height = 200  Width = 400'#13#10 +
    '  object VINCatalog: TOBDVINCatalog AutoLoad = True Left = 32 Top = 24 end'#13#10 +
    '  object DriveCycleCatalog: TOBDDriveCycleCatalogComp AutoLoad = True Left = 32 Top = 80 end'#13#10 +
    '  object EVCatalog: TOBDEVBatteryCatalogComp AutoLoad = True Left = 32 Top = 136 end'#13#10 +
    'end'#13#10;
var Pas, Dfm: TOBDStarterArtifact;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Pas := Default(TOBDStarterArtifact);
  Pas.RelativePath := AContext.UnitName + '.pas';
  Pas.Content := Expand(PAS_TPL, AContext);
  Result[1] := Pas;
  Dfm := Default(TOBDStarterArtifact);
  Dfm.RelativePath := AContext.UnitName + '.dfm';
  Dfm.Content := Expand(DFM_TPL, AContext);
  Result[2] := Dfm;
end;

{ ---- Sessions / Transports -------------------------------------------------- }

function GenerateSessionsForm(const AContext: TOBDStarterContext): TArray<TOBDStarterArtifact>;
const
  PAS_TPL =
    'unit {UNIT};'#13#10#13#10 +
    'interface'#13#10#13#10 +
    'uses'#13#10 +
    '  System.SysUtils, System.Classes,'#13#10 +
    '  Vcl.Controls, Vcl.Forms,'#13#10 +
    '  OBD.Protocol.KWP1281,'#13#10 +
    '  OBD.Protocol.KWP1281.Session;'#13#10#13#10 +
    'type'#13#10 +
    '  {FORM} = class(TForm)'#13#10 +
    '    Session: TOBDKWP1281Session;'#13#10 +
    '    procedure FormCreate(Sender: TObject);'#13#10 +
    '  end;'#13#10#13#10 +
    'var {FORMVAR}: {FORM};'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'procedure {FORM}.FormCreate(Sender: TObject);'#13#10 +
    'begin'#13#10 +
    '  // Hand a transport to the session before calling Connect.'#13#10 +
    '  // Pick the implementation that matches your hardware:'#13#10 +
    '  //   * TKWP1281SerialTransport  - USB-K-line (FTDI / PL2303)'#13#10 +
    '  //   * TKWP1281ELMTransport     - ELM327 in slow-init mode'#13#10 +
    '  //   * TKWP1281TP20Transport    - TP2.0 over CAN (2003+ VAG)'#13#10 +
    '  //   * TKWP1281ISOTPTransport   - ISO-TP gateway'#13#10 +
    '  //   * TKWP1281J2534Transport   - J2534 PassThru DLL'#13#10 +
    '  // Session.SetTransport(MyTransport);'#13#10 +
    '  // Session.Connect;'#13#10 +
    'end;'#13#10#13#10 +
    'end.'#13#10;
  DFM_TPL =
    'object {FORMVAR}: {FORM}'#13#10 +
    '  Caption = ''{PROJ} - KWP1281 session'''#13#10 +
    '  ClientHeight = 200 ClientWidth = 360 Position = poScreenCenter'#13#10 +
    '  OnCreate = FormCreate'#13#10 +
    '  object Session: TOBDKWP1281Session Address = $56 ByteTimeoutMs = 1000 Left = 24 Top = 24 end'#13#10 +
    'end'#13#10;
var Pas, Dfm: TOBDStarterArtifact;
begin
  SetLength(Result, 3);
  Result[0] := MakeDpr(AContext);
  Pas := Default(TOBDStarterArtifact);
  Pas.RelativePath := AContext.UnitName + '.pas';
  Pas.Content := Expand(PAS_TPL, AContext);
  Result[1] := Pas;
  Dfm := Default(TOBDStarterArtifact);
  Dfm.RelativePath := AContext.UnitName + '.dfm';
  Dfm.Content := Expand(DFM_TPL, AContext);
  Result[2] := Dfm;
end;

procedure RegisterAll;
var S: TOBDStarter;
begin
  S := Default(TOBDStarter);
  S.Id := 'radio-calculator-minimal';
  S.Title := 'Radio-code calculator (minimal)';
  S.Description :=
    'Bare radio-code calculator form: serial input + Calculate ' +
    'button + result label. Use this when you don''t want the ' +
    'logo / About / status-bar chrome from the polished template.';
  S.Category := 'Radio';
  S.Generate := GenerateRadioForm;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'eeprom-extractor';
  S.Title := 'EEPROM extractor';
  S.Description :=
    'Loads a .bin / .eep dump and extracts the unlock code at ' +
    'the documented offset. The form ships with one of the three ' +
    'bundled extractors dropped in - swap if your radio uses a ' +
    'different chip / offset.';
  S.Category := 'EEPROM';
  S.Generate := GenerateEEPROMForm;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'catalogs-dm';
  S.Title := 'Catalogue manager DataModule';
  S.Description :=
    'TDataModule with TOBDVINCatalog + TOBDDriveCycleCatalogComp + ' +
    'TOBDEVBatteryCatalogComp pre-wired (AutoLoad = True). Drop on ' +
    'a project to centralise catalogue setup.';
  S.Category := 'Catalogs';
  S.Generate := GenerateCatalogsDM;
  TOBDStarterRegistry.Default.Register(S);

  S := Default(TOBDStarter);
  S.Id := 'kwp1281-session';
  S.Title := 'KWP1281 session form';
  S.Description :=
    'Form with a TOBDKWP1281Session pre-configured for VW radios ' +
    '(address $56). FormCreate is the wire-up point for the ' +
    'transport (Serial / ELM / TP2.0 / ISO-TP / J2534) - the ' +
    'generated code lists each option in a comment block.';
  S.Category := 'Sessions';
  S.Generate := GenerateSessionsForm;
  TOBDStarterRegistry.Default.Register(S);
end;

initialization
  RegisterAll;

end.
