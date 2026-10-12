; OBD-Studio Personal - Inno Setup 6 script
;
; Build the application first (Release, Win64 or Win32), then compile this
; script with Inno Setup 6:
;
;   iscc applications\obd-studio-personal\installer\OBDStudioPersonal.iss
;   iscc /DPlatform=Win32 applications\obd-studio-personal\installer\OBDStudioPersonal.iss
;
; The setup installs per user by default (no administrator rights needed).
; It installs the program, the catalogues, the library and application
; language files, the licence and the data source notices. Settings stay
; in %APPDATA%\ERDesigns\OBD-Studio Personal, and logs, reports,
; recordings and dashboards in Documents\OBD-Studio Personal. Uninstalling
; leaves both alone, because they are the user's data.

#define AppName      "OBD-Studio Personal"
#define AppPublisher "ERDesigns"
#define AppURL       "https://erdesigns.eu"
#define AppExe       "OBDStudioPersonal.exe"
#ifndef AppVersion
  #define AppVersion "0.1.0"
#endif
#ifndef Platform
  #define Platform "Win64"
#endif
#ifndef Config
  #define Config "Release"
#endif

#define AppDir  ".."
#define RepoDir "..\..\.."
#define BinDir  AppDir + "\" + Platform + "\" + Config

[Setup]
AppId={{7F9F1441-227B-4024-8D29-C8EE930DD668}
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
AppPublisher={#AppPublisher}
AppPublisherURL={#AppURL}
AppSupportURL={#AppURL}
VersionInfoVersion={#AppVersion}
DefaultDirName={autopf}\{#AppPublisher}\{#AppName}
DefaultGroupName={#AppName}
DisableProgramGroupPage=yes
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
LicenseFile={#AppDir}\EULA.md
OutputDir={#AppDir}\installer\Output
OutputBaseFilename=OBDStudioPersonal-{#AppVersion}-{#Platform}-setup
UninstallDisplayIcon={app}\{#AppExe}
Compression=lzma2/max
SolidCompression=yes
WizardStyle=modern
MinVersion=10.0
ShowLanguageDialog=auto
UsePreviousLanguage=no
#if Platform == "Win64"
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
#endif

; Wizard languages: English plus every European translation that the
; installed Inno Setup ships. Each one is only used when its file exists,
; so the script compiles with any Inno Setup 6 release. The program itself
; has its own catalogues for all 38 European languages.
[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
#if FileExists(CompilerPath + "Languages\Bulgarian.isl")
Name: "bg"; MessagesFile: "compiler:Languages\Bulgarian.isl"
#endif
#if FileExists(CompilerPath + "Languages\Catalan.isl")
Name: "ca"; MessagesFile: "compiler:Languages\Catalan.isl"
#endif
#if FileExists(CompilerPath + "Languages\Czech.isl")
Name: "cs"; MessagesFile: "compiler:Languages\Czech.isl"
#endif
#if FileExists(CompilerPath + "Languages\Danish.isl")
Name: "da"; MessagesFile: "compiler:Languages\Danish.isl"
#endif
#if FileExists(CompilerPath + "Languages\German.isl")
Name: "de"; MessagesFile: "compiler:Languages\German.isl"
#endif
#if FileExists(CompilerPath + "Languages\Spanish.isl")
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"
#endif
#if FileExists(CompilerPath + "Languages\Finnish.isl")
Name: "fi"; MessagesFile: "compiler:Languages\Finnish.isl"
#endif
#if FileExists(CompilerPath + "Languages\French.isl")
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"
#endif
#if FileExists(CompilerPath + "Languages\Hungarian.isl")
Name: "hu"; MessagesFile: "compiler:Languages\Hungarian.isl"
#endif
#if FileExists(CompilerPath + "Languages\Icelandic.isl")
Name: "is"; MessagesFile: "compiler:Languages\Icelandic.isl"
#endif
#if FileExists(CompilerPath + "Languages\Italian.isl")
Name: "it"; MessagesFile: "compiler:Languages\Italian.isl"
#endif
#if FileExists(CompilerPath + "Languages\Lithuanian.isl")
Name: "lt"; MessagesFile: "compiler:Languages\Lithuanian.isl"
#endif
#if FileExists(CompilerPath + "Languages\Norwegian.isl")
Name: "nb"; MessagesFile: "compiler:Languages\Norwegian.isl"
#endif
#if FileExists(CompilerPath + "Languages\Dutch.isl")
Name: "nl"; MessagesFile: "compiler:Languages\Dutch.isl"
#endif
#if FileExists(CompilerPath + "Languages\Polish.isl")
Name: "pl"; MessagesFile: "compiler:Languages\Polish.isl"
#endif
#if FileExists(CompilerPath + "Languages\Portuguese.isl")
Name: "pt"; MessagesFile: "compiler:Languages\Portuguese.isl"
#endif
#if FileExists(CompilerPath + "Languages\Russian.isl")
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"
#endif
#if FileExists(CompilerPath + "Languages\Slovak.isl")
Name: "sk"; MessagesFile: "compiler:Languages\Slovak.isl"
#endif
#if FileExists(CompilerPath + "Languages\Slovenian.isl")
Name: "sl"; MessagesFile: "compiler:Languages\Slovenian.isl"
#endif
#if FileExists(CompilerPath + "Languages\Swedish.isl")
Name: "sv"; MessagesFile: "compiler:Languages\Swedish.isl"
#endif
#if FileExists(CompilerPath + "Languages\Turkish.isl")
Name: "tr"; MessagesFile: "compiler:Languages\Turkish.isl"
#endif
#if FileExists(CompilerPath + "Languages\Ukrainian.isl")
Name: "uk"; MessagesFile: "compiler:Languages\Ukrainian.isl"
#endif

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "{#BinDir}\{#AppExe}"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#AppDir}\EULA.md"; DestDir: "{app}"; Flags: ignoreversion
Source: "{#AppDir}\README.md"; DestDir: "{app}"; Flags: ignoreversion
; Library catalogues (PIDs, DTC libraries, inspection rules, ranges) and
; their licence notices.
Source: "{#RepoDir}\catalogs\*.json"; DestDir: "{app}\catalogs"; Flags: ignoreversion recursesubdirs createallsubdirs; Excludes: "_schema\*"
Source: "{#RepoDir}\catalogs\SOURCES.md"; DestDir: "{app}\catalogs"; Flags: ignoreversion
; Library language files, then the application's own strings.
Source: "{#RepoDir}\languages\*.json"; DestDir: "{app}\languages"; Flags: ignoreversion
Source: "{#AppDir}\languages\*.json"; DestDir: "{app}\languages\personal"; Flags: ignoreversion

[InstallDelete]
; Catalogue and language files are replaced as a set on upgrade, so a file
; dropped from a release does not linger.
Type: filesandordirs; Name: "{app}\catalogs"
Type: filesandordirs; Name: "{app}\languages"

[Icons]
Name: "{autoprograms}\{#AppName}"; Filename: "{app}\{#AppExe}"
Name: "{autodesktop}\{#AppName}"; Filename: "{app}\{#AppExe}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#AppExe}"; Description: "{cm:LaunchProgram,{#StringChange(AppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent
