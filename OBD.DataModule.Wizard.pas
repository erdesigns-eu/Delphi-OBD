//------------------------------------------------------------------------------
// UNIT           : OBD.DataModule.Wizard.pas
// CONTENTS       : OBD DataModule wizard
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 05/04/2024
//------------------------------------------------------------------------------
unit OBD.DataModule.Wizard;

interface

uses
  System.SysUtils, WinApi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, PlatformAPI,

  OBD.Mainform.Wizard;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD DataModule Creator Wizard
  /// </summary>
  TOBDataModuleCreatorWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTAFormWizard, IOTARepositoryWizard60, IOTARepositoryWizard80, IOTARepositoryWizard160)
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
    // IOTARepositoryWizard160
    function GetFrameworkTypes: TArray<string>;
    function GetPlatforms: TArray<string>;
  end;

  /// <summary>
  ///   OBD DataModule Creator
  /// </summary>
  TOBDDataModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FProject: IOTAProject;
  public
    constructor Create; overload; virtual;
    constructor Create(AProject: IOTAProject); overload; virtual;

    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

implementation

//------------------------------------------------------------------------------
// VARIABLES
//------------------------------------------------------------------------------
var
  /// <summary>
  ///   Wizard Icon
  /// </summary>
  WizardIcon: HICON;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET ID STRING
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetIDString: string;
begin
  Result := 'ERDesigns.OBDDataModuleWizard';
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET NAME
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetName: string;
begin
  Result := 'ERDesigns OBD DataModule';
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET STATE
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: EXECUTE
//------------------------------------------------------------------------------
procedure TOBDataModuleCreatorWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TOBDDataModuleCreator.Create);
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET AUTHOR
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetAuthor: string;
begin
  Result := 'ERDesigns';
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET COMMENT
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetComment: string;
begin
  Result := 'Creates a new ERDesigns OBD DataModule.';
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET PAGE
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetPage: string;
begin
  Result := 'ERDesigns OBD';
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET GLYPH
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetGlyph: Cardinal;
begin
  Result := WizardIcon;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET DESIGNER
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET PERSONALITY
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET GALLERY CATEGORY
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetGalleryCategory: IOTAGalleryCategory;
var
  Category: IOTAGalleryCategory;
  CatManager: IOTAGalleryCategoryManager;
begin
  CatManager := (BorlandIDEServices as IOTAGalleryCategoryManager);
  Assert(Assigned(CatManager));
  Category := CatManager.FindCategory(sCategoryDelphiNewFiles);
  if Assigned(Category) then
    Result := Category
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET FRAMEWORK TYPES
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetFrameworkTypes: TArray<String>;
begin
  Setlength(Result, 1);
  Result[0] := sFrameworkTypeVCL;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR WIZARD: GET PLATFORMS
//------------------------------------------------------------------------------
function TOBDataModuleCreatorWizard.GetPlatforms: TArray<String>;
begin
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: FORM CREATED
//------------------------------------------------------------------------------
procedure TOBDDataModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Nothing
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET ANCESTOR NAME
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetAncestorName: string;
begin
  Result := 'OBDDataModule';
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDDataModuleCreator.Create;
begin
  // Call inherited constructor
  inherited Create;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: CONSTRUCTOR
//------------------------------------------------------------------------------
constructor TOBDDataModuleCreator.Create(AProject: IOTAProject);
begin
  // Call inherited constructor
  inherited Create;
  // Store project
  FProject := AProject;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET ANCESTOR NAME
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetCreatorType: string;
begin
  // Return sUnit or sText as appropriate
  Result := sForm;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET EXCISTING
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET FILE SYSTEM
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET FORM NAME
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetFormName: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET IMPLEMENTATION FILENAME
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET INTERFACE FILENAME
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetIntfFileName: string;
begin
  // Blank for forms
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET MAIN FORM
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetMainForm: Boolean;
begin
  // Since this is not a mainform, return false.
  Result := False;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET OWNER
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetOwner: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  NewModule: IOTAModule;
begin
  if Assigned(FProject) then
  begin
    Result := FProject;
  end else
  begin
    Result := nil;
    ModuleServices := (BorlandIDEServices as IOTAModuleServices);
    Module := ModuleServices.CurrentModule;

    if Module <> nil then
    begin
      if Module.QueryInterface(IOTAProject, NewModule) = S_OK then
        Result := NewModule

      else if Module.OwnerModuleCount > 0 then
      begin
        NewModule := Module.OwnerModules[0];
        if NewModule <> nil then
          if NewModule.QueryInterface(IOTAProject, Result) <> S_OK then
            Result := nil;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET SHOW FORM
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetShowForm: Boolean;
begin
  // Show the form by default if it is not part of the Project Wizard.
  Result := not Assigned(FProject);
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET SHOW SOURCE
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetShowSource: Boolean;
begin
  // Show the source code by default if it is not part of the Project Wizard.
  Result := not Assigned(FProject);
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: GET UNNAMED
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.GetUnnamed: Boolean;
begin
  // Typically True for new files to prompt for a name
  Result := True;
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: NEW FORM FILE
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
const
  FormDFMTemplate =
    'object %s: T%s'                                         + #13#10 +
    '  Height = 480'                                         + #13#10 +
    '  Width = 640'                                          + #13#10 +
    '  object OBDConnectionComponent1: TOBDConnectionComponent' + #13#10 +
    '    ConnectionType = ctSerial'                          + #13#10 +
    '    SerialPort = ''COM1'''                              + #13#10 +
    '    SerialBaudRate = br38400'                           + #13#10 +
    '    Left = 40'                                          + #13#10 +
    '    Top = 40'                                           + #13#10 +
    '  end'                                                  + #13#10 +
    '  object OBDProtocolComponent1: TOBDProtocolComponent'  + #13#10 +
    '    ConnectionComponent = OBDConnectionComponent1'      + #13#10 +
    '    AutoBindConnection = True'                          + #13#10 +
    '    Left = 200'                                         + #13#10 +
    '    Top = 40'                                           + #13#10 +
    '  end'                                                  + #13#10 +
    'end';
begin
  Result := TOBDSourceFile.Create(Format(FormDFMTemplate, [FormIdent, FormIdent]))
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: NEW IMPLEMENATION SOURCE
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
const
  SourceTemplate =
    '//------------------------------------------------------------------------------'  + #13#10 +
    '// UNIT           : %s.pas'                                                        + #13#10 +
    '// VERSION        : 1.0'                                                           + #13#10 +
    '// TARGET         : Embarcadero Delphi 11 or higher'                               + #13#10 +
    '// AUTHOR         : Ernst Reidinga (ERDesigns)'                                    + #13#10 +
    '// STATUS         : Open source under Apache 2.0 library'                          + #13#10 +
    '// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11'                                      + #13#10 +
    '// CREATED DATE   : %s'                                                            + #13#10 +
    '//------------------------------------------------------------------------------'  + #13#10 +
    ''                                                                                  + #13#10 +
    'unit %s;'                                                                          + #13#10 +
    ''                                                                                  + #13#10 +
    'interface'                                                                         + #13#10 +
    ''                                                                                  + #13#10 +
    'uses'                                                                              + #13#10 +
    '  System.SysUtils, System.Classes,'                                                + #13#10 +

    '  OBD.DataModule, OBD.Connection.Component, OBD.Protocol.Component;'              + #13#10 +

    'type'                                                                              + #13#10 +
    '  T%s = class(T%s)'                                                                + #13#10 +

    // Shared OBD components
    '    OBDConnectionComponent1: TOBDConnectionComponent;'                             + #13#10 +
    '    OBDProtocolComponent1: TOBDProtocolComponent;'                                 + #13#10 +
    // Shared OBD components

    '  private'                                                                         + #13#10 +
    '    { Private declarations }'                                                      + #13#10 +
    '  public'                                                                          + #13#10 +
    '    { Public declarations }'                                                       + #13#10 +
    '  end;'                                                                            + #13#10 +
    'var'                                                                               + #13#10 +
    '  %s: T%s;'                                                                        + #13#10 +
    ''                                                                                  + #13#10 +
    'implementation'                                                                    + #13#10 +
    ''                                                                                  + #13#10 +
    '{%%CLASSGROUP ''Vcl.Controls.TControl''}'                                          + #13#10 +
    ''                                                                                  + #13#10 +
    '{$R *.dfm}'                                                                        + #13#10 +
    ''                                                                                  + #13#10 +
    'end.';
begin
  Result := TOBDSourceFile.Create(Format(SourceTemplate, [ModuleIdent, FormatDateTime('dd/mm/yyyy', Now), ModuleIdent, FormIdent, AncestorIdent, FormIdent, FormIdent]));
end;

//------------------------------------------------------------------------------
// TOBDDATAMODULE CREATOR: NEW INTERFACE SOURCE
//------------------------------------------------------------------------------
function TOBDDataModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

//------------------------------------------------------------------------------
// INITIALIZATION
//------------------------------------------------------------------------------
initialization
  // Load the wizard icon
  WizardIcon := LoadIcon(hInstance, 'DATAMODULE');

end.
