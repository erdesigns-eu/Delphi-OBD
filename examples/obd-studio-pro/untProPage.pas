//------------------------------------------------------------------------------
//  untProPage
//
//  The contract between the OBD Studio Pro main form and its pages.
//
//  Every page is a TFrame of its own unit, designed in the form designer.
//  The main form creates the frames at run time inside pnlPages and talks
//  to them only through IProPage; a page reaches the main form only
//  through IProHost (toasts, the session log, the status bar, a themed
//  confirmation dialog and the simulated vehicle). Pages therefore do not
//  use untMain and can be added, removed or reused on their own.
//
//  Both interfaces are implemented by TComponent descendants, whose
//  reference counting is switched off: holding an IProHost in a frame
//  does not keep the form alive or free it.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//------------------------------------------------------------------------------

unit untProPage;

interface

uses
  ERD.UI.Toast,
  ERD.UI.Dialogs,
  untSimulator;

type
  /// <summary>Services the main form offers its pages.</summary>
  IProHost = interface
    ['{F898E16F-2008-43C9-8B1C-EBDD8522EB5D}']
    /// <summary>The simulated vehicle; values are in metric units.</summary>
    function Simulator: TEngineSimulator;
    /// <summary>True while the (simulated) adapter is connected.</summary>
    function Connected: Boolean;
    /// <summary>Shows a toast.</summary>
    procedure Notify(AKind: TOBDToastKind; const ATitle, AText: string);
    /// <summary>Adds an information line to the session log.</summary>
    procedure Log(const AText: string);
    /// <summary>Adds a warning line to the session log.</summary>
    procedure LogWarning(const AText: string);
    /// <summary>Shows a long-running job in the status bar: APercent
    /// 0..100 shows the progress panel with AText; a negative APercent
    /// hides it.</summary>
    procedure SetProgress(const AText: string; APercent: Integer);
    /// <summary>Asks a themed yes / cancel question; True when the user
    /// chose AConfirmCaption.</summary>
    function Confirm(AKind: TOBDDialogKind; const ATitle, AText,
      AConfirmCaption: string): Boolean;
  end;

  /// <summary>One page of the main form.</summary>
  IProPage = interface
    ['{E57A6369-8510-4001-AE05-E79DBC92211D}']
    /// <summary>Called once, after the frame is created and parented.
    /// Fill the controls with the sample data here.</summary>
    procedure Init(const AHost: IProHost);
    /// <summary>Called every 100 ms, also while the page is hidden, so
    /// jobs such as a flash keep running in the background. Check
    /// AHost.Connected before using vehicle data.</summary>
    procedure Tick;
    /// <summary>Called after the adapter connected or disconnected.</summary>
    procedure ConnectionChanged;
  end;

implementation

end.
