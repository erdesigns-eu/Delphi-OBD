//------------------------------------------------------------------------------
// UNIT           : OBD.Project.Wizard.pas
// CONTENTS       : OBD Project wizard
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 04/04/2024
//------------------------------------------------------------------------------
unit OBD.Project.Wizard;

interface

uses
  System.SysUtils, WinApi.Windows, Vcl.Dialogs, DesignIntf, ToolsAPI, PlatformAPI,
  OBD.Mainform.Wizard;

//------------------------------------------------------------------------------
// CLASSES
//------------------------------------------------------------------------------
type
  /// <summary>
  ///   OBD Project Creator Wizard
  /// </summary>
  TOBDProjectModuleCreatorWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTARepositoryWizard60, IOTARepositoryWizard80, IOTARepositoryWizard160, IOTAProjectWizard, IOTAProjectWizard100)
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
    // IOTAProjectWizard100
    function IsVisible(Project: IOTAProject): Boolean;
  end;

  /// <summary>
  ///   OBD Project Creator
  /// </summary>
  TOBDProjectCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator, IOTAProjectCreator50, IOTAProjectCreator80, IOTAProjectCreator160)
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    // IOTAProjectCreator80
    function GetProjectPersonality: string;
    // IOTAProjectCreator160
    function GetFrameworkType: string;
    function GetPlatforms: TArray<string>;
    function GetPreferredPlatform: string;
    procedure SetInitialOptions(const NewProject: IOTAProject);
  end;

implementation

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET ID STRING
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetIDString: string;
begin
  Result := 'ERDesigns.OBDProjectWizard';
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET NAME
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetName: string;
begin
  Result := 'OBD Project';
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET STATE
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: EXECUTE
//------------------------------------------------------------------------------
procedure TOBDProjectModuleCreatorWizard.Execute;
begin
  (BorlandIDEServices as IOTAModuleServices).CreateModule(TOBDProjectCreator.Create);
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET AUTHOR
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetAuthor: string;
begin
  Result := 'ERDesigns';
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET COMMENT
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetComment: string;
begin
  Result := 'Create a new ERDesigns OBD Project, including a OBD MainForm with a header, subheader and statusbar.';
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET PAGE
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetPage: string;
begin
  Result := 'ERDesigns OBD';
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET GLYPH
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET DESIGNER
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET PERSONALITY
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET GALLERY CATEGORY
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetGalleryCategory: IOTAGalleryCategory;
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
// TOBPROJECT MODULE CREATOR WIZARD: GET FRAMEWORK TYPES
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetFrameworkTypes: TArray<String>;
begin
  Setlength(Result, 1);
  Result[0] := sFrameworkTypeVCL;
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: GET PLATFORMS
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.GetPlatforms: TArray<String>;
begin
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;
end;

//------------------------------------------------------------------------------
// TOBPROJECT MODULE CREATOR WIZARD: IS VISIBLE
//------------------------------------------------------------------------------
function TOBDProjectModuleCreatorWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := True;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET CREATOR TYPE
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetCreatorType: string;
begin
  // Create an application project
  Result := sApplication;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET EXISTING
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetExisting: Boolean;
begin
  // Create a new project
  Result := False;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET FILENAME
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetFileName: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET FILE SYSTEM
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET OPTION FILENAME
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET CURRENT PROJECT GROUP
//------------------------------------------------------------------------------
function GetCurrentProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    if IModule.QueryInterface(IOTAProjectGroup, IProjectGroup) = S_OK then
    begin
      Result := IProjectGroup;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET OWNER
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetOwner: IOTAModule;
begin
  // Owned by current project group
  Result := GetCurrentProjectGroup;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET SHOW SOURCE
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetShowSource: Boolean;
begin
  // Dont show the application source code by default
  Result := False;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET UNNAMED
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetUnnamed: Boolean;
begin
  // Typically True for new files to prompt for a name
  Result := True;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: NEW DEFAULT MODULE
//------------------------------------------------------------------------------
procedure TOBDProjectCreator.NewDefaultModule;
begin
  // Deprecated: Implement and use the method on IOTAProjectCreator50.
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: NEW OPTION SOUCRCE
//------------------------------------------------------------------------------
function TOBDProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  // For BCB only
  Result := nil;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: NEW PROJECT RESOURCE
//------------------------------------------------------------------------------
procedure TOBDProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  // No resources needed
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: NEW PROJECT SOURCE
//------------------------------------------------------------------------------
function TOBDProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
const
  ProjectTemplate =
    '//------------------------------------------------------------------------------' + #13#10 +
    '// PROJECT        : %s'                                                           + #13#10 +
    '// VERSION        : 1.0'                                                          + #13#10 +
    '// TARGET         : Embarcadero Delphi 11 or higher'                              + #13#10 +
    '// AUTHOR         : Ernst Reidinga (ERDesigns)'                                   + #13#10 +
    '// STATUS         : Open source under Apache 2.0 library'                         + #13#10 +
    '// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11'                                     + #13#10 +
    '// CREATED DATE   : %s'                                                           + #13#10 +
    '//------------------------------------------------------------------------------' + #13#10 +
    ''                                                                                 + #13#10 +
    'program %s;'                                                                      + #13#10 +
    ''                                                                                 + #13#10 +
    'uses'                                                                             + #13#10 +
    '  Vcl.Forms;'                                                                     + #13#10 +
    ''                                                                                 + #13#10 +
    '{$R *.res}'                                                                       + #13#10 +
    ''                                                                                 + #13#10 +
    'begin'                                                                            + #13#10 +
    '  Application.Initialize;'                                                        + #13#10 +
    '  Application.MainFormOnTaskbar := True;'                                         + #13#10 +
    '  Application.Run;'                                                               + #13#10 +
    'end.';
begin
  // Create the default source code for a new OBD application.
  Result := TOBDSourceFile.Create(Format(ProjectTemplate, [ProjectName, FormatDateTime('dd/mm/yyyy', Now), ProjectName]));
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: NEW PROJECT MODULE
//------------------------------------------------------------------------------
procedure TOBDProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
var
  ModuleServices: IOTAModuleServices;
  MainForm: IOTAModule;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if Assigned(ModuleServices) then
  begin
    MainForm := ModuleServices.CreateModule(TOBDMainFormModuleCreator.Create(Project));
  end;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET PROJECT PERSONALITY
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET FRAMEWORK TYPE
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetFrameworkType: string;
begin
  Result := sFrameworkTypeVCL;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET PLATFORMS
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetPlatforms: TArray<String>;
begin
  SetLength(Result, 2);
  Result[0] := cWin32Platform;
  Result[1] := cWin64Platform;;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: GET PREFERRED PLATFORM
//------------------------------------------------------------------------------
function TOBDProjectCreator.GetPreferredPlatform: string;
begin
  Result := cWin32Platform;
end;

//------------------------------------------------------------------------------
// TOBDPROJECT CREATOR: SET INITIAL OPTIONS
//------------------------------------------------------------------------------
procedure TOBDProjectCreator.SetInitialOptions(const NewProject: IOTAProject);
begin
  //
end;

end.
