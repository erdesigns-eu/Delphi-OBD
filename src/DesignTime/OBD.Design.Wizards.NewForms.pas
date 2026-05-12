//------------------------------------------------------------------------------
//  OBD.Design.Wizards.NewForms
//
//  Three Tools-API "new form / DM" wizards under the
//  File > New > Other... -> Delphi-OBD page. Each scaffolds a
//  single new unit + form in the host's current project,
//  pre-wired with the v2 OBD-component plumbing the matching
//  workflow needs:
//
//    - TOBDNewFormWizard:       Form (Connection + Adapter +
//                               Protocol non-visuals on a fresh
//                               TForm).
//    - TOBDNewDataModuleWizard: Headless TDataModule with the
//                               same trio so a host can keep
//                               the OBD plumbing on a shared
//                               DM and consume it from any form.
//    - TOBDNewMainFormWizard:   Main form: Connection + Adapter
//                               + Protocol + a TOBDLiveData and
//                               a TOBDDTCs, plus a TOBDCircularGauge
//                               bound to PID 0x0C (engine RPM) so
//                               the form runs out-of-the-box.
//
//  Generated templates use the v2 component names (TOBDConnection,
//  TOBDAdapter, TOBDProtocol, TOBDLiveData, TOBDDTCs,
//  TOBDCircularGauge). The v1 wizards referenced
//  TOBDConnectionComponent / TOBDProtocolComponent / TOBDTouchHeader
//  / TOBDTouchSubheader / TOBDTouchStatusbar — those v1 names do
//  not exist in v2, so the v2 templates use the v2 surface.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-11  ERD  Initial port from v1 OBD.Form.Wizard /
//                     OBD.DataModule.Wizard / OBD.Mainform.Wizard,
//                     rewired to v2 component names.
//------------------------------------------------------------------------------

unit OBD.Design.Wizards.NewForms;

interface

uses
  System.SysUtils,
  System.Classes,
  ToolsAPI;

type
  /// <summary>
  ///   Shared <c>IOTAFile</c> wrapper. The IDE pulls source text
  ///   through this interface; we hold the rendered string in
  ///   memory.
  /// </summary>
  TOBDWizardSourceFile = class(TInterfacedObject, IOTAFile)
  strict private
    FSource: string;
  public
    /// <summary>Constructs a source file from a rendered template.</summary>
    /// <param name="ASource">Full text the IDE will write to disk.</param>
    constructor Create(const ASource: string);
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  /// <summary>
  ///   Base "new module creator" implementation shared by all three
  ///   wizards. Subclasses override <see cref="RenderForm"/> /
  ///   <see cref="RenderImpl"/> and a couple of metadata getters.
  /// </summary>
  TOBDNewModuleCreatorBase = class(TInterfacedObject,
    IOTACreator, IOTAModuleCreator)
  strict protected
    function GetAncestorName: string; virtual; abstract;
    function GetFormTemplate(const AFormIdent: string): string; virtual; abstract;
    function GetImplTemplate(const AUnitIdent: string;
      const AFormIdent: string;
      const AAncestorIdent: string): string; virtual; abstract;
    function GetIsMainForm: Boolean; virtual;
  public
    // IOTACreator
    function GetCreatorType: string; virtual;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent: string;
      const AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent: string;
      const FormIdent: string;
      const AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent: string;
      const FormIdent: string;
      const AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  /// <summary>New Form module creator — scaffolds a standard
  /// <c>TForm</c> with the OBD connection trio.</summary>
  TOBDNewFormCreator = class(TOBDNewModuleCreatorBase)
  strict protected
    function GetAncestorName: string; override;
    function GetFormTemplate(const AFormIdent: string): string; override;
    function GetImplTemplate(const AUnitIdent: string;
      const AFormIdent: string;
      const AAncestorIdent: string): string; override;
  end;

  /// <summary>New DataModule creator — scaffolds a
  /// <c>TDataModule</c> with the OBD connection trio.</summary>
  TOBDNewDataModuleCreator = class(TOBDNewModuleCreatorBase)
  strict protected
    function GetAncestorName: string; override;
    function GetFormTemplate(const AFormIdent: string): string; override;
    function GetImplTemplate(const AUnitIdent: string;
      const AFormIdent: string;
      const AAncestorIdent: string): string; override;
  public
    function GetCreatorType: string; override;
  end;

  /// <summary>New MainForm creator — scaffolds the project's main
  /// form with the OBD trio plus a TOBDLiveData / TOBDDTCs /
  /// TOBDCircularGauge starter set.</summary>
  TOBDNewMainFormCreator = class(TOBDNewModuleCreatorBase)
  strict protected
    function GetAncestorName: string; override;
    function GetFormTemplate(const AFormIdent: string): string; override;
    function GetImplTemplate(const AUnitIdent: string;
      const AFormIdent: string;
      const AAncestorIdent: string): string; override;
    function GetIsMainForm: Boolean; override;
  end;

  /// <summary>
  ///   New-Form wizard registered under the Delphi-OBD repository
  ///   page.
  /// </summary>
  TOBDNewFormWizard = class(TNotifierObject,
    IOTAWizard, IOTARepositoryWizard, IOTAFormWizard,
    IOTARepositoryWizard60, IOTARepositoryWizard80)
  public
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
    // IOTARepositoryWizard60
    function GetDesigner: string;
    // IOTARepositoryWizard80
    function GetPersonality: string;
    function GetGalleryCategory: IOTAGalleryCategory;
  end;

  /// <summary>New-DataModule wizard.</summary>
  TOBDNewDataModuleWizard = class(TNotifierObject,
    IOTAWizard, IOTARepositoryWizard, IOTAFormWizard,
    IOTARepositoryWizard60, IOTARepositoryWizard80)
  public
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
    function GetDesigner: string;
    function GetPersonality: string;
    function GetGalleryCategory: IOTAGalleryCategory;
  end;

  /// <summary>New-MainForm wizard.</summary>
  TOBDNewMainFormWizard = class(TNotifierObject,
    IOTAWizard, IOTARepositoryWizard, IOTAFormWizard,
    IOTARepositoryWizard60, IOTARepositoryWizard80)
  public
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
    function GetDesigner: string;
    function GetPersonality: string;
    function GetGalleryCategory: IOTAGalleryCategory;
  end;

/// <summary>
///   Registers all three "new form / DM" wizards with the IDE.
///   Idempotent — safe to call from any design-time
///   <c>Register</c> path.
/// </summary>
procedure RegisterDelphiOBDFormWizards;

implementation

uses
  PlatformAPI;

{ ---- TOBDWizardSourceFile -------------------------------------------------- }

constructor TOBDWizardSourceFile.Create(const ASource: string);
begin
  inherited Create;
  FSource := ASource;
end;

function TOBDWizardSourceFile.GetSource: string;
begin
  Result := FSource;
end;

function TOBDWizardSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

{ ---- TOBDNewModuleCreatorBase --------------------------------------------- }

function TOBDNewModuleCreatorBase.GetCreatorType: string;
begin
  Result := sForm;
end;

function TOBDNewModuleCreatorBase.GetExisting: Boolean;
begin
  Result := False;
end;

function TOBDNewModuleCreatorBase.GetFileSystem: string;
begin
  Result := '';
end;

function TOBDNewModuleCreatorBase.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TOBDNewModuleCreatorBase.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TOBDNewModuleCreatorBase.GetImplFileName: string;
begin
  Result := '';
end;

function TOBDNewModuleCreatorBase.GetIntfFileName: string;
begin
  Result := '';
end;

function TOBDNewModuleCreatorBase.GetFormName: string;
begin
  Result := '';
end;

function TOBDNewModuleCreatorBase.GetIsMainForm: Boolean;
begin
  Result := False;
end;

function TOBDNewModuleCreatorBase.GetMainForm: Boolean;
begin
  Result := GetIsMainForm;
end;

function TOBDNewModuleCreatorBase.GetShowForm: Boolean;
begin
  Result := True;
end;

function TOBDNewModuleCreatorBase.GetShowSource: Boolean;
begin
  Result := True;
end;

function TOBDNewModuleCreatorBase.NewFormFile(const FormIdent: string;
  const AncestorIdent: string): IOTAFile;
begin
  Result := TOBDWizardSourceFile.Create(GetFormTemplate(FormIdent));
end;

function TOBDNewModuleCreatorBase.NewImplSource(const ModuleIdent: string;
  const FormIdent: string;
  const AncestorIdent: string): IOTAFile;
begin
  Result := TOBDWizardSourceFile.Create(
    GetImplTemplate(ModuleIdent, FormIdent, AncestorIdent));
end;

function TOBDNewModuleCreatorBase.NewIntfSource(const ModuleIdent: string;
  const FormIdent: string;
  const AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TOBDNewModuleCreatorBase.FormCreated(
  const FormEditor: IOTAFormEditor);
begin
  // No-op — the IDE wires the form on its own.
end;

{ ---- TOBDNewFormCreator --------------------------------------------------- }

function TOBDNewFormCreator.GetAncestorName: string;
begin
  Result := 'TForm';
end;

function TOBDNewFormCreator.GetFormTemplate(
  const AFormIdent: string): string;
const
  FormDFM =
    'object %0:s: T%0:s'                                + #13#10 +
    '  Left = 0'                                        + #13#10 +
    '  Top = 0'                                         + #13#10 +
    '  Caption = ''%0:s'''                              + #13#10 +
    '  ClientHeight = 442'                              + #13#10 +
    '  ClientWidth = 628'                               + #13#10 +
    '  Color = clBtnFace'                               + #13#10 +
    '  Font.Charset = DEFAULT_CHARSET'                  + #13#10 +
    '  Font.Color = clWindowText'                       + #13#10 +
    '  Font.Height = -12'                               + #13#10 +
    '  Font.Name = ''Segoe UI'''                        + #13#10 +
    '  Font.Style = []'                                 + #13#10 +
    '  TextHeight = 15'                                 + #13#10 +
    '  object Connection: TOBDConnection'               + #13#10 +
    '    Left = 24; Top = 16'                           + #13#10 +
    '  end'                                             + #13#10 +
    '  object Adapter: TOBDAdapter'                     + #13#10 +
    '    Connection = Connection'                       + #13#10 +
    '    Left = 96; Top = 16'                           + #13#10 +
    '  end'                                             + #13#10 +
    '  object Protocol: TOBDProtocol'                   + #13#10 +
    '    Adapter = Adapter'                             + #13#10 +
    '    Left = 168; Top = 16'                          + #13#10 +
    '  end'                                             + #13#10 +
    'end';
begin
  Result := Format(FormDFM, [AFormIdent]);
end;

function TOBDNewFormCreator.GetImplTemplate(const AUnitIdent: string;
  const AFormIdent: string;
  const AAncestorIdent: string): string;
const
  ImplPas =
    'unit %0:s;'                                        + #13#10 +
    ''                                                  + #13#10 +
    'interface'                                         + #13#10 +
    ''                                                  + #13#10 +
    'uses'                                              + #13#10 +
    '  System.SysUtils, System.Classes, Vcl.Controls,'  + #13#10 +
    '  Vcl.Forms,'                                      + #13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol;'      + #13#10 +
    ''                                                  + #13#10 +
    'type'                                              + #13#10 +
    '  T%1:s = class(TForm)'                            + #13#10 +
    '    Connection: TOBDConnection;'                   + #13#10 +
    '    Adapter: TOBDAdapter;'                         + #13#10 +
    '    Protocol: TOBDProtocol;'                       + #13#10 +
    '  end;'                                            + #13#10 +
    ''                                                  + #13#10 +
    'var'                                               + #13#10 +
    '  %1:s: T%1:s;'                                    + #13#10 +
    ''                                                  + #13#10 +
    'implementation'                                    + #13#10 +
    ''                                                  + #13#10 +
    '{$R *.dfm}'                                        + #13#10 +
    ''                                                  + #13#10 +
    'end.'                                              + #13#10;
begin
  Result := Format(ImplPas, [AUnitIdent, AFormIdent]);
end;

{ ---- TOBDNewDataModuleCreator --------------------------------------------- }

function TOBDNewDataModuleCreator.GetCreatorType: string;
begin
  Result := sDataModule;
end;

function TOBDNewDataModuleCreator.GetAncestorName: string;
begin
  Result := 'TDataModule';
end;

function TOBDNewDataModuleCreator.GetFormTemplate(
  const AFormIdent: string): string;
const
  DfmTemplate =
    'object %0:s: T%0:s'                                + #13#10 +
    '  OldCreateOrder = False'                          + #13#10 +
    '  Height = 240'                                    + #13#10 +
    '  Width = 320'                                     + #13#10 +
    '  object Connection: TOBDConnection'               + #13#10 +
    '    Left = 24; Top = 16'                           + #13#10 +
    '  end'                                             + #13#10 +
    '  object Adapter: TOBDAdapter'                     + #13#10 +
    '    Connection = Connection'                       + #13#10 +
    '    Left = 96; Top = 16'                           + #13#10 +
    '  end'                                             + #13#10 +
    '  object Protocol: TOBDProtocol'                   + #13#10 +
    '    Adapter = Adapter'                             + #13#10 +
    '    Left = 168; Top = 16'                          + #13#10 +
    '  end'                                             + #13#10 +
    'end';
begin
  Result := Format(DfmTemplate, [AFormIdent]);
end;

function TOBDNewDataModuleCreator.GetImplTemplate(
  const AUnitIdent: string;
  const AFormIdent: string;
  const AAncestorIdent: string): string;
const
  ImplPas =
    'unit %0:s;'                                        + #13#10 +
    ''                                                  + #13#10 +
    'interface'                                         + #13#10 +
    ''                                                  + #13#10 +
    'uses'                                              + #13#10 +
    '  System.SysUtils, System.Classes,'                + #13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol;'      + #13#10 +
    ''                                                  + #13#10 +
    'type'                                              + #13#10 +
    '  T%1:s = class(TDataModule)'                      + #13#10 +
    '    Connection: TOBDConnection;'                   + #13#10 +
    '    Adapter: TOBDAdapter;'                         + #13#10 +
    '    Protocol: TOBDProtocol;'                       + #13#10 +
    '  end;'                                            + #13#10 +
    ''                                                  + #13#10 +
    'var'                                               + #13#10 +
    '  %1:s: T%1:s;'                                    + #13#10 +
    ''                                                  + #13#10 +
    'implementation'                                    + #13#10 +
    ''                                                  + #13#10 +
    '{$R *.dfm}'                                        + #13#10 +
    ''                                                  + #13#10 +
    'end.'                                              + #13#10;
begin
  Result := Format(ImplPas, [AUnitIdent, AFormIdent]);
end;

{ ---- TOBDNewMainFormCreator ----------------------------------------------- }

function TOBDNewMainFormCreator.GetAncestorName: string;
begin
  Result := 'TForm';
end;

function TOBDNewMainFormCreator.GetIsMainForm: Boolean;
begin
  Result := True;
end;

function TOBDNewMainFormCreator.GetFormTemplate(
  const AFormIdent: string): string;
const
  DfmTemplate =
    'object %0:s: T%0:s'                                + #13#10 +
    '  Left = 0'                                        + #13#10 +
    '  Top = 0'                                         + #13#10 +
    '  Caption = ''%0:s'''                              + #13#10 +
    '  ClientHeight = 480'                              + #13#10 +
    '  ClientWidth = 800'                               + #13#10 +
    '  Color = clBtnFace'                               + #13#10 +
    '  Font.Charset = DEFAULT_CHARSET'                  + #13#10 +
    '  Font.Color = clWindowText'                       + #13#10 +
    '  Font.Height = -12'                               + #13#10 +
    '  Font.Name = ''Segoe UI'''                        + #13#10 +
    '  Font.Style = []'                                 + #13#10 +
    '  TextHeight = 15'                                 + #13#10 +
    '  object CircularGauge1: TOBDCircularGauge'        + #13#10 +
    '    Left = 280; Top = 80'                          + #13#10 +
    '    Width = 240; Height = 240'                     + #13#10 +
    '    LiveData = LiveData'                           + #13#10 +
    '    PID = $0C'                                     + #13#10 +
    '  end'                                             + #13#10 +
    '  object Connection: TOBDConnection'               + #13#10 +
    '    Left = 24; Top = 416'                          + #13#10 +
    '  end'                                             + #13#10 +
    '  object Adapter: TOBDAdapter'                     + #13#10 +
    '    Connection = Connection'                       + #13#10 +
    '    Left = 96; Top = 416'                          + #13#10 +
    '  end'                                             + #13#10 +
    '  object Protocol: TOBDProtocol'                   + #13#10 +
    '    Adapter = Adapter'                             + #13#10 +
    '    Left = 168; Top = 416'                          + #13#10 +
    '  end'                                             + #13#10 +
    '  object LiveData: TOBDLiveData'                   + #13#10 +
    '    Protocol = Protocol'                           + #13#10 +
    '    Left = 240; Top = 416'                         + #13#10 +
    '  end'                                             + #13#10 +
    '  object DTCs: TOBDDTCs'                           + #13#10 +
    '    Protocol = Protocol'                           + #13#10 +
    '    Left = 312; Top = 416'                         + #13#10 +
    '  end'                                             + #13#10 +
    'end';
begin
  Result := Format(DfmTemplate, [AFormIdent]);
end;

function TOBDNewMainFormCreator.GetImplTemplate(
  const AUnitIdent: string;
  const AFormIdent: string;
  const AAncestorIdent: string): string;
const
  ImplPas =
    'unit %0:s;'                                        + #13#10 +
    ''                                                  + #13#10 +
    'interface'                                         + #13#10 +
    ''                                                  + #13#10 +
    'uses'                                              + #13#10 +
    '  System.SysUtils, System.Classes, Vcl.Controls,'  + #13#10 +
    '  Vcl.Forms,'                                      + #13#10 +
    '  OBD.Connection, OBD.Adapter, OBD.Protocol,'      + #13#10 +
    '  OBD.Service.LiveData, OBD.Service.DTCs,'         + #13#10 +
    '  OBD.UI.Gauges.Dial;'                             + #13#10 +
    ''                                                  + #13#10 +
    'type'                                              + #13#10 +
    '  T%1:s = class(TForm)'                            + #13#10 +
    '    Connection: TOBDConnection;'                   + #13#10 +
    '    Adapter: TOBDAdapter;'                         + #13#10 +
    '    Protocol: TOBDProtocol;'                       + #13#10 +
    '    LiveData: TOBDLiveData;'                       + #13#10 +
    '    DTCs: TOBDDTCs;'                               + #13#10 +
    '    CircularGauge1: TOBDCircularGauge;'            + #13#10 +
    '  end;'                                            + #13#10 +
    ''                                                  + #13#10 +
    'var'                                               + #13#10 +
    '  %1:s: T%1:s;'                                    + #13#10 +
    ''                                                  + #13#10 +
    'implementation'                                    + #13#10 +
    ''                                                  + #13#10 +
    '{$R *.dfm}'                                        + #13#10 +
    ''                                                  + #13#10 +
    'end.'                                              + #13#10;
begin
  Result := Format(ImplPas, [AUnitIdent, AFormIdent]);
end;

{ ---- Wizard metadata helpers ---------------------------------------------- }

function CommonAuthor: string;
begin
  Result := 'ERDesigns';
end;

function CommonPage: string;
begin
  Result := 'Delphi-OBD';
end;

function CommonDesigner: string;
begin
  Result := dVCL;
end;

function CommonPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function CommonGalleryCategory: IOTAGalleryCategory;
begin
  Result := nil;
end;

{ ---- TOBDNewFormWizard ---------------------------------------------------- }

function TOBDNewFormWizard.GetIDString: string;
begin
  Result := 'ERDesigns.DelphiOBD.NewForm';
end;

function TOBDNewFormWizard.GetName: string;
begin
  Result := 'Delphi-OBD Form';
end;

function TOBDNewFormWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TOBDNewFormWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    TOBDNewFormCreator.Create);
end;

function TOBDNewFormWizard.GetAuthor: string;
begin
  Result := CommonAuthor;
end;

function TOBDNewFormWizard.GetComment: string;
begin
  Result := 'Adds a new VCL form pre-wired with TOBDConnection / ' +
            'TOBDAdapter / TOBDProtocol.';
end;

function TOBDNewFormWizard.GetPage: string;
begin
  Result := CommonPage;
end;

function TOBDNewFormWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TOBDNewFormWizard.GetDesigner: string;
begin
  Result := CommonDesigner;
end;

function TOBDNewFormWizard.GetPersonality: string;
begin
  Result := CommonPersonality;
end;

function TOBDNewFormWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := CommonGalleryCategory;
end;

{ ---- TOBDNewDataModuleWizard ---------------------------------------------- }

function TOBDNewDataModuleWizard.GetIDString: string;
begin
  Result := 'ERDesigns.DelphiOBD.NewDataModule';
end;

function TOBDNewDataModuleWizard.GetName: string;
begin
  Result := 'Delphi-OBD DataModule';
end;

function TOBDNewDataModuleWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TOBDNewDataModuleWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    TOBDNewDataModuleCreator.Create);
end;

function TOBDNewDataModuleWizard.GetAuthor: string;
begin
  Result := CommonAuthor;
end;

function TOBDNewDataModuleWizard.GetComment: string;
begin
  Result := 'Adds a headless TDataModule pre-wired with ' +
            'TOBDConnection / TOBDAdapter / TOBDProtocol so ' +
            'every form in the project can share one OBD stack.';
end;

function TOBDNewDataModuleWizard.GetPage: string;
begin
  Result := CommonPage;
end;

function TOBDNewDataModuleWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TOBDNewDataModuleWizard.GetDesigner: string;
begin
  Result := CommonDesigner;
end;

function TOBDNewDataModuleWizard.GetPersonality: string;
begin
  Result := CommonPersonality;
end;

function TOBDNewDataModuleWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := CommonGalleryCategory;
end;

{ ---- TOBDNewMainFormWizard ----------------------------------------------- }

function TOBDNewMainFormWizard.GetIDString: string;
begin
  Result := 'ERDesigns.DelphiOBD.NewMainForm';
end;

function TOBDNewMainFormWizard.GetName: string;
begin
  Result := 'Delphi-OBD MainForm';
end;

function TOBDNewMainFormWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TOBDNewMainFormWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(
    TOBDNewMainFormCreator.Create);
end;

function TOBDNewMainFormWizard.GetAuthor: string;
begin
  Result := CommonAuthor;
end;

function TOBDNewMainFormWizard.GetComment: string;
begin
  Result := 'Scaffolds the project main form with the full OBD ' +
            'starter set — TOBDConnection / TOBDAdapter / ' +
            'TOBDProtocol plus TOBDLiveData, TOBDDTCs and a ' +
            'TOBDCircularGauge.';
end;

function TOBDNewMainFormWizard.GetPage: string;
begin
  Result := CommonPage;
end;

function TOBDNewMainFormWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TOBDNewMainFormWizard.GetDesigner: string;
begin
  Result := CommonDesigner;
end;

function TOBDNewMainFormWizard.GetPersonality: string;
begin
  Result := CommonPersonality;
end;

function TOBDNewMainFormWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := CommonGalleryCategory;
end;

procedure RegisterDelphiOBDFormWizards;
begin
  RegisterPackageWizard(TOBDNewFormWizard.Create);
  RegisterPackageWizard(TOBDNewDataModuleWizard.Create);
  RegisterPackageWizard(TOBDNewMainFormWizard.Create);
end;

end.
