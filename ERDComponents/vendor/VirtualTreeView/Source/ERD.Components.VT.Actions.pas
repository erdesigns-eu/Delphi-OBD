unit ERD.Components.VT.Actions;

// ERD adaptation: namespaced VCL engine; original attribution is retained below.
// Distributed under the selected Mozilla Public License 1.1.
// Full terms and upstream provenance: ../LICENSE.MPL-1.1.txt and ../upstream.json.


interface

uses
  System.Classes,
  System.Actions,
  Vcl.Controls,
  Vcl.ActnList,
  ERD.Components.VT.Types,
  ERD.Components.VT.BaseTree;

type
  TERDVTVirtualTreeAction = class(TCustomAction)
  strict private
    fTree: TERDVTBaseVirtualTree; // Member variable for the property "Control"
    fTreeAutoDetect: Boolean; // True if a potential Virtual TreeView should be detected automatically, false if a specific Tree was assigned to the property "Tree"
    fOnAfterExecute: TNotifyEvent; // Member variable for the OnAfterExecute event
    function GetSelectedOnly: Boolean; // Setter for the property "SelectedOnly"
    procedure SetSelectedOnly(const Value: Boolean); // Getter for the property "SelectedOnly"
  strict protected
    fFilter: TVirtualNodeStates; // Apply only of nodes which match these states
    procedure SetControl(Value: TERDVTBaseVirtualTree); // Setter for the property "Control"
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoAfterExecute; virtual;// Fires the event "OnAfterExecute"
    property SelectedOnly: Boolean read GetSelectedOnly write SetSelectedOnly default False;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    property Control: TERDVTBaseVirtualTree read fTree write SetControl;
    property OnAfterExecute: TNotifyEvent read fOnAfterExecute write fOnAfterExecute; // Executed after the action was performed
    property Caption;
    property Enabled;
    property HelpContext;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnHint;
  end;

  TERDVTVirtualTreePerItemAction = class(TERDVTVirtualTreeAction)
  strict private
    fOnBeforeExecute: TNotifyEvent;
    fOldCursor: TCursor;
  strict protected
    fToExecute: TVTGetNodeProc; // method which is executed per item to perform this action
    procedure DoBeforeExecute();
    procedure DoAfterExecute(); override;// Fires the event "OnAfterExecute"
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property OnBeforeExecute: TNotifyEvent read fOnBeforeExecute write fOnBeforeExecute;
  end;

  // A standard action which checkmarks nodes in a virtual treeview
  TERDVTVirtualTreeCheckAll = class(TERDVTVirtualTreePerItemAction)
  protected
    fDesiredCheckState: TCheckState;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SelectedOnly;
    property OnUpdate;
  end;

  // A standard action which unchecks nodes in a virtual treeview
  TERDVTVirtualTreeUncheckAll = class(TERDVTVirtualTreeCheckAll)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TERDVTVirtualTreeSelectAll = class(TERDVTVirtualTreeAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  // Base class for actions that are applied to selected nodes only
  TERDVTVirtualTreeForSelectedAction = class(TERDVTVirtualTreeAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TERDVTVirtualTreeCopy = class(TERDVTVirtualTreeForSelectedAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TERDVTVirtualTreeCut = class(TERDVTVirtualTreeForSelectedAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TERDVTVirtualTreePaste = class(TERDVTVirtualTreeForSelectedAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TERDVTVirtualTreeDelete = class(TERDVTVirtualTreeForSelectedAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

procedure Register;


implementation

uses
  WinApi.Windows,
  Vcl.Forms;

procedure Register;
begin
  RegisterActions('VirtualTree', [TERDVTVirtualTreeCheckAll, TERDVTVirtualTreeUncheckAll, TERDVTVirtualTreeSelectAll, TERDVTVirtualTreeCopy, TERDVTVirtualTreeCut, TERDVTVirtualTreePaste, TERDVTVirtualTreeDelete], nil);
end;

{ TVirtualTreeAction }

constructor TERDVTVirtualTreeAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTree := nil;
  fFilter := [];
  fOnAfterExecute := nil;
  fTreeAutoDetect := True;
end;

function TERDVTVirtualTreeAction.GetSelectedOnly: Boolean;
begin
  exit(TVirtualNodeState.vsSelected in fFilter);
end;

procedure TERDVTVirtualTreeAction.SetSelectedOnly(const Value: Boolean);
begin
  if Value then
    Include(fFilter, TVirtualNodeState.vsSelected)
  else
    Exclude(fFilter, TVirtualNodeState.vsSelected);
end;

procedure TERDVTVirtualTreeAction.DoAfterExecute;
begin
  if Assigned(fOnAfterExecute) then
    fOnAfterExecute(Self);
end;

function TERDVTVirtualTreeAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Target is TERDVTBaseVirtualTree);
end;

function TERDVTVirtualTreeAction.Update(): Boolean;
begin
  Result := inherited;
  // If an OnUpdate event handler is assigned, TBasicAction.Update() will return True and so TBasicAction.UpdateTarget() will not be called.
  // We would then end up with Control == nil. So trigger update of action manually.
  if Result and Assigned(OnUpdate) then
    SendAppMessage(CM_ACTIONUPDATE, 0, LPARAM(Self))
end;

procedure TERDVTVirtualTreeAction.UpdateTarget(Target: TObject);
begin
  if fTreeAutoDetect and (Target is TERDVTBaseVirtualTree) then
    fTree := (Target as TERDVTBaseVirtualTree);
  Enabled := Assigned(Control) and not Control.IsEmpty and (not SelectedOnly or (Control.SelectedCount > 0))
end;

procedure TERDVTVirtualTreeAction.ExecuteTarget(Target: TObject);
begin
  DoAfterExecute();
end;

procedure TERDVTVirtualTreeAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FTree) then
    FTree := nil;
end;

procedure TERDVTVirtualTreeAction.SetControl(Value: TERDVTBaseVirtualTree);
begin
  if Value <> fTree then begin
    fTree := Value;
    if Assigned(fTree) then begin
      fTree.FreeNotification(Self);// register Self as a component that should be notified when fTree is about to be destroyed.
    end;//if
    // Do not update the target of this action if it wa set explicitely by the developer
    fTreeAutoDetect := not Assigned(fTree);
  end;//if
end;


{ TVirtualTreePerItemAction }

constructor TERDVTVirtualTreePerItemAction.Create(AOwner: TComponent);
begin
  inherited;
  fToExecute := nil;
  fOnBeforeExecute := nil;
  fOldCursor := crNone;
end;

procedure TERDVTVirtualTreePerItemAction.DoAfterExecute;
begin
  inherited;
  if fOldCursor <> crNone then
    Screen.Cursor := fOldCursor;
end;

procedure TERDVTVirtualTreePerItemAction.DoBeforeExecute;
begin
  if Screen.Cursor <> crHourGlass then begin
    fOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
  end;//if
  if Assigned(fOnBeforeExecute) then
    fOnBeforeExecute(Self);
end;

procedure TERDVTVirtualTreePerItemAction.ExecuteTarget(Target: TObject);
begin
  DoBeforeExecute();
  Control.BeginUpdate();
  try
    Control.IterateSubtree(nil, Self.fToExecute, nil, fFilter, true);
  finally
    Control.EndUpdate();
    DoAfterExecute();
  end;
end;

{ TVirtualTreeCheckAll }

constructor TERDVTVirtualTreeCheckAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hint := 'Check all items in the list';
  Caption := 'Check &All';
  fDesiredCheckState := csCheckedNormal;
  fToExecute := procedure(Sender: TERDVTBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
                begin
                  if not Control.CheckState[Node].IsDisabled then
                    Control.CheckState[Node] := fDesiredCheckState;
                end;
end;


{ TVirtualTreeUncheckAll }

constructor TERDVTVirtualTreeUncheckAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hint := 'Uncheck all items in the list';
  Caption := '&Uncheck All';
  fDesiredCheckState := csUncheckedNormal;
end;


{ TVirtualStringSelectAll }

procedure TERDVTVirtualTreeSelectAll.UpdateTarget(Target: TObject);
begin
  Inherited;
  //Enabled := Enabled and (toMultiSelect in Control.TreeOptions.SelectionOptions)  // TreeOptions is protected  :-(
end;

procedure TERDVTVirtualTreeSelectAll.ExecuteTarget(Target: TObject);
begin
  Control.SelectAll(False);
  inherited;
end;


{ TVirtualTreeForSelectedAction }

constructor TERDVTVirtualTreeForSelectedAction.Create(AOwner: TComponent);
begin
  inherited;
  SelectedOnly := True;
end;


{ TVirtualTreeCopy }

procedure TERDVTVirtualTreeCopy.ExecuteTarget(Target: TObject);
begin
  Control.CopyToClipboard();
  Inherited;
end;


{ TVirtualTreeCut }

procedure TERDVTVirtualTreeCut.ExecuteTarget(Target: TObject);
begin
  Control.CutToClipboard();
  Inherited;
end;


{ TVirtualTreePaste }

procedure TERDVTVirtualTreePaste.ExecuteTarget(Target: TObject);
begin
  Control.PasteFromClipboard();
  Inherited;
end;


{ TVirtualTreeDelete }

procedure TERDVTVirtualTreeDelete.ExecuteTarget(Target: TObject);
begin
  Control.DeleteSelectedNodes();
  Inherited;
end;


end.
