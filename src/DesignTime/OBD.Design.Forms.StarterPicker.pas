//------------------------------------------------------------------------------
//  OBD.Design.Forms.StarterPicker
//
//  TOBDStarterPickerDlg — modal picker invoked by the Delphi-OBD
//  starter wizard. Shows the registered starters grouped by
//  category, lets the host pick one, and collects the project /
//  unit / form names plus the target directory before handing
//  control back to the wizard.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation.
//------------------------------------------------------------------------------

unit OBD.Design.Forms.StarterPicker;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  OBD.Design.Starters;

type
  /// <summary>Modal starter picker.</summary>
  TOBDStarterPickerDlg = class(TForm)
    pnlLeft: TPanel;
    lblPickHeader: TLabel;
    tvStarters: TTreeView;
    splPane: TSplitter;
    pnlRight: TPanel;
    lblTitle: TLabel;
    lblDescription: TLabel;
    pnlInputs: TPanel;
    lblTarget: TLabel;
    edtTargetDir: TEdit;
    btnBrowse: TButton;
    lblProj: TLabel;
    edtProj: TEdit;
    lblUnit: TLabel;
    edtUnit: TEdit;
    pnlButtons: TPanel;
    btnCreate: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure tvStartersChange(Sender: TObject; Node: TTreeNode);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure edtProjChange(Sender: TObject);
  strict private
    FSelected: TOBDStarter;
    FHasSelection: Boolean;
    procedure SelectStarter(const AStarter: TOBDStarter);
    procedure UpdateUnitFromProject;
  public
    /// <summary>Shows the picker. Returns <c>True</c> when the
    /// user clicked "Create"; populates <c>AContext</c> and
    /// <c>AStarter</c> on success.</summary>
    class function Pick(out AContext: TOBDStarterContext;
      out AStarter: TOBDStarter): Boolean; static;
  end;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  Vcl.FileCtrl;

{ ---- TOBDStarterPickerDlg --------------------------------------------------- }

procedure TOBDStarterPickerDlg.FormCreate(Sender: TObject);
var
  All: TArray<TOBDStarter>;
  I: Integer;
  CategoryNode, StarterNode: TTreeNode;
  CurrentCategory: string;
begin
  All := TOBDStarterRegistry.Default.All;
  tvStarters.Items.BeginUpdate;
  try
    tvStarters.Items.Clear;
    CategoryNode := nil;
    CurrentCategory := '';
    for I := 0 to High(All) do
    begin
      if (CategoryNode = nil) or (All[I].Category <> CurrentCategory) then
      begin
        CategoryNode := tvStarters.Items.AddChild(nil, All[I].Category);
        CurrentCategory := All[I].Category;
      end;
      StarterNode := tvStarters.Items.AddChildObject(
        CategoryNode, All[I].Title, Pointer(NativeInt(I) + 1));
      // Stash the index in the Data pointer so we can recover the
      // record on selection (without holding a managed reference).
    end;
    tvStarters.FullExpand;
    if (Length(All) > 0) and (tvStarters.Items.Count > 0) then
      tvStarters.Selected := tvStarters.Items[0];
  finally
    tvStarters.Items.EndUpdate;
  end;

  // Default target dir: <documents>\Delphi-OBD\<projname>.
  edtProj.Text := 'MyDelphiOBDProject';
  UpdateUnitFromProject;
  edtTargetDir.Text := TPath.Combine(
    TPath.GetDocumentsPath, 'Delphi-OBD');
end;

procedure TOBDStarterPickerDlg.tvStartersChange(Sender: TObject;
  Node: TTreeNode);
var
  Idx: Integer;
  All: TArray<TOBDStarter>;
begin
  if (Node = nil) or (Node.Data = nil) then
  begin
    lblTitle.Caption := '(select a starter)';
    lblDescription.Caption := '';
    FHasSelection := False;
    btnCreate.Enabled := False;
    Exit;
  end;
  Idx := NativeInt(Node.Data) - 1;
  All := TOBDStarterRegistry.Default.All;
  if (Idx < 0) or (Idx > High(All)) then Exit;
  SelectStarter(All[Idx]);
end;

procedure TOBDStarterPickerDlg.SelectStarter(const AStarter: TOBDStarter);
begin
  FSelected := AStarter;
  FHasSelection := True;
  lblTitle.Caption := AStarter.Title;
  lblDescription.Caption := AStarter.Description;
  btnCreate.Enabled := True;
end;

procedure TOBDStarterPickerDlg.btnBrowseClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := edtTargetDir.Text;
  if SelectDirectory('Select target folder', '', Dir) then
    edtTargetDir.Text := Dir;
end;

procedure TOBDStarterPickerDlg.edtProjChange(Sender: TObject);
begin
  UpdateUnitFromProject;
end;

procedure TOBDStarterPickerDlg.UpdateUnitFromProject;
begin
  // Default unit name: 'MainForm' — keep it stable. Hosts can
  // override.
  if Trim(edtUnit.Text) = '' then
    edtUnit.Text := 'MainForm';
end;

procedure TOBDStarterPickerDlg.btnCreateClick(Sender: TObject);
begin
  if not FHasSelection then
  begin
    MessageDlg('Pick a starter first.', mtInformation, [mbOK], 0);
    Exit;
  end;
  if Trim(edtTargetDir.Text) = '' then
  begin
    MessageDlg('Target folder is required.', mtError, [mbOK], 0);
    Exit;
  end;
  if Trim(edtProj.Text) = '' then
  begin
    MessageDlg('Project name is required.', mtError, [mbOK], 0);
    Exit;
  end;
  if Trim(edtUnit.Text) = '' then
    edtUnit.Text := 'MainForm';
  ModalResult := mrOk;
end;

class function TOBDStarterPickerDlg.Pick(
  out AContext: TOBDStarterContext;
  out AStarter: TOBDStarter): Boolean;
var
  Dlg: TOBDStarterPickerDlg;
  ProjDir: string;
begin
  Result := False;
  Dlg := TOBDStarterPickerDlg.Create(nil);
  try
    if (Dlg.ShowModal = mrOk) and Dlg.FHasSelection then
    begin
      AStarter := Dlg.FSelected;
      // Each project gets its own subfolder under the target dir
      // so a .dpr next to other projects isn't a problem.
      ProjDir := TPath.Combine(Dlg.edtTargetDir.Text,
        Trim(Dlg.edtProj.Text));
      AContext.TargetDir        := ProjDir;
      AContext.ProjectName      := Trim(Dlg.edtProj.Text);
      AContext.UnitName         := Trim(Dlg.edtUnit.Text);
      AContext.FormClassName    := 'T' + AContext.UnitName;
      AContext.FormInstanceName := AContext.UnitName;
      Result := True;
    end;
  finally
    Dlg.Free;
  end;
end;

end.
