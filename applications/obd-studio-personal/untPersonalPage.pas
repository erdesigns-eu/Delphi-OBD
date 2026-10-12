//------------------------------------------------------------------------------
//  untPersonalPage
//
//  The contract between the OBD-Studio Personal main form and its pages.
//
//  Every page is a TFrame of its own unit. The main form creates the
//  frames at run time inside pnlPages and talks to them only through
//  IPersonalPage; a page reaches the main form only through IPersonalHost
//  (the session with the vehicle, the settings, toasts, the activity log,
//  the status bar and a themed confirmation dialog). Pages therefore do
//  not use untMain and can be added, removed or reused on their own.
//
//  A page with buttons also implements IPersonalCommands: the main form
//  then offers those buttons as commands in the ribbon (a coloured tab
//  for the active page) and the menu bar (a menu for the active page).
//
//  The interfaces are implemented by TComponent descendants, whose
//  reference counting is switched off: holding an IPersonalHost in a
//  frame does not keep the form alive or free it.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see EULA.md
//------------------------------------------------------------------------------

unit untPersonalPage;

interface

uses
  System.Classes,
  ERD.UI.Toast,
  ERD.UI.Dialogs,
  untSettings,
  untSession;

const
  // Page keys. They are stable: the settings remember the last page by
  // key, the welcome wizard and the overview jump to pages by key.
  PAGE_OVERVIEW    = 'overview';
  PAGE_CODES       = 'codes';
  PAGE_READINESS   = 'readiness';
  PAGE_LIVE        = 'live';
  PAGE_DASHBOARD   = 'dashboard';
  PAGE_TESTS       = 'tests';
  PAGE_INFO        = 'info';
  PAGE_FUEL        = 'fuel';
  PAGE_PERFORMANCE = 'performance';
  PAGE_EV          = 'ev';
  PAGE_ADAPTER     = 'adapter';
  PAGE_RECORDINGS  = 'recordings';
  PAGE_CONSOLE     = 'console';
  PAGE_SETTINGS    = 'settings';

type
  /// <summary>The page buttons the main form also offers in the ribbon
  /// and the menu bar. Each command belongs to exactly one page.</summary>
  TPersonalCommand = (
    // Overview
    pcScanVehicle, pcSaveVehicleReport,
    // Trouble codes
    pcReadCodes, pcClearCodes, pcLookUpCode, pcSaveCodeReport,
    // Readiness
    pcRefreshReadiness, pcStartAdvisor, pcStopAdvisor,
    // Live data
    pcRefreshSensors, pcRecordLive,
    // Dashboard
    pcAddGauge, pcEditDashboard, pcSaveDashboard, pcResetDashboard,
    // Monitor tests
    pcReadTests, pcExportTests,
    // Vehicle info
    pcReadVehicleInfo, pcCopyVehicleInfo, pcSaveVehicleInfo,
    // Fuel economy
    pcResetTrip,
    // Performance
    pcArmRun, pcCancelRun,
    // EV battery
    pcReadBattery,
    // Adapter
    pcRefreshPorts,
    // Recordings
    pcRecordSession, pcRefreshRecordings, pcRenameRecording,
    pcDeleteRecording, pcExportRecording, pcShowRecording,
    // Console
    pcClearConsole, pcSaveConsole);
  TPersonalCommands = set of TPersonalCommand;

  /// <summary>Services the main form offers its pages.</summary>
  IPersonalHost = interface
    ['{0B6E2F3C-6E59-4F0B-9C8A-2D1F7B54A311}']
    /// <summary>The vehicle session: connection, adapter, protocol and
    /// every diagnostic service, plus the background job queue.</summary>
    function Session: TPersonalSession;
    /// <summary>The user settings (saved as JSON under %APPDATA%).</summary>
    function Settings: TPersonalSettings;
    /// <summary>True while an adapter (or the demo car) is connected.</summary>
    function Connected: Boolean;
    /// <summary>Shows a toast.</summary>
    procedure Notify(AKind: TOBDToastKind; const ATitle, AText: string);
    /// <summary>Adds a line to the activity log (Tools - Console, and
    /// Logs\activity-yyyymmdd.log in the data folder).</summary>
    procedure Log(const AText: string);
    /// <summary>Adds a warning to the activity log.</summary>
    procedure LogWarning(const AText: string);
    /// <summary>The activity log of this run, oldest line first.</summary>
    function ActivityLog: TStrings;
    /// <summary>Shows a progress bar in the status bar; a negative
    /// percentage hides it.</summary>
    procedure SetProgress(const AText: string; APercent: Integer);
    /// <summary>Asks a yes/cancel question in the themed dialog; dkDanger
    /// paints the confirm button red.</summary>
    function Confirm(AKind: TOBDDialogKind; const ATitle, AText,
      AConfirmCaption: string): Boolean;
    /// <summary>Shows the page with the given key (PAGE_*).</summary>
    procedure ShowPage(const AKey: string);
    /// <summary>Sets the badge next to a page in the sidebar; 0 hides it.</summary>
    procedure SetBadge(const AKey: string; ACount: Integer);
    /// <summary>Tells the status bar how many codes are stored and whether
    /// the check engine light is on.</summary>
    procedure SetCodeSummary(ACount: Integer; AMIL: Boolean);
    /// <summary>Opens the connect page, or disconnects when connected.</summary>
    procedure ToggleConnection;
    /// <summary>Applies and saves the settings after a page changed them
    /// (theme, density, units, language, inspection country).</summary>
    procedure SettingsChanged;
    /// <summary>Folder with the language catalogues (languages\ next to
    /// the program); pass it to OBDI18n.AvailableLanguages.</summary>
    function LanguageFolder: string;
    /// <summary>Shows the red recording indicator in the status bar with
    /// AText (e.g. 'REC 02:14'); an empty text hides it.</summary>
    procedure SetRecording(const AText: string);
  end;

  /// <summary>Implemented by every page frame.</summary>
  IPersonalPage = interface
    ['{4C0A9D1E-58B7-4E6A-A3F2-91D0C6B8E5F4}']
    /// <summary>The PAGE_* key of this page.</summary>
    function PageKey: string;
    /// <summary>Called once, right after the frame is created.</summary>
    procedure Init(const AHost: IPersonalHost);
    /// <summary>Called each time the page becomes visible (True) or is
    /// hidden (False). Pages start and stop their polling here.</summary>
    procedure Activated(AActive: Boolean);
    /// <summary>Called on every connect and disconnect.</summary>
    procedure ConnectionChanged(AConnected: Boolean);
    /// <summary>Called when the user changes the unit system.</summary>
    procedure UnitsChanged;
    /// <summary>Called once after Init and again after every language
    /// change: sets every user-visible caption through
    /// Tr('personal.&lt;page&gt;.&lt;name&gt;', 'English text'). DFM captions
    /// are only design-time placeholders.</summary>
    procedure Translate;
  end;

  /// <summary>Implemented by pages whose buttons are also offered as
  /// commands in the ribbon and the menu bar.</summary>
  IPersonalCommands = interface
    ['{EAB06835-FE62-4825-92ED-74057F6AB682}']
    /// <summary>The commands of this page.</summary>
    function Commands: TPersonalCommands;
    /// <summary>True when the command can run now: the same rule as the
    /// Enabled state of the page's own button. The main form asks while
    /// the application is idle, also while the page is hidden.</summary>
    function CommandEnabled(ACommand: TPersonalCommand): Boolean;
    /// <summary>True while an on/off command is on (recording, editing
    /// the layout); shown as a check mark or a pressed ribbon button.</summary>
    function CommandChecked(ACommand: TPersonalCommand): Boolean;
    /// <summary>Runs the command as if the user pressed the page's own
    /// button. The main form shows the page first, unless the command
    /// also works in the background (recording a session).</summary>
    procedure ExecuteCommand(ACommand: TPersonalCommand);
  end;

implementation

end.
