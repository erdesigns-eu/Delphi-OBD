//------------------------------------------------------------------------------
//  OBD.Design.Forms.StarterPicker
//
//  TOBDStarterPickerDlg — modal Windows-style multi-step wizard
//  invoked by the Delphi-OBD starter Tools-API entry. Three
//  steps:
//
//    1. Starter        — pick from the registered TTreeView,
//                        grouped by category.
//    2. Options        — built dynamically from the selected
//                        starter's <c>OptionGroups</c>; skipped
//                        when the starter has no options.
//    3. Project / unit — target folder, project name, unit name.
//
//  Bottom strip carries Back / Next / Finish / Cancel buttons,
//  with state managed in <see cref="UpdateNavigation"/>. The
//  options page is rebuilt every time the host enters it so a
//  Back-then-Next cycle picks up a different starter cleanly.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  History     :
//    2026-05-10  ERD  Initial implementation as a single-page picker.
//    2026-05-10  ERD  Rebuild as multi-step wizard with dynamic
//                     option groups.
//------------------------------------------------------------------------------

unit OBD.Design.Forms.StarterPicker;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  OBD.Design.Starters;

type
  /// <summary>Wizard page identifier.</summary>
  TOBDStarterStep = (sssStarter, sssOptions, sssTarget);

  /// <summary>Modal multi-step starter wizard.</summary>
  TOBDStarterPickerDlg = class(TForm)
    pnlContent: TPanel;
    pnlStepStarter: TPanel;
    pnlStepStarterLeft: TPanel;
    lblPickHeader: TLabel;
    tvStarters: TTreeView;
    splPane: TSplitter;
    pnlStepStarterRight: TPanel;
    lblTitle: TLabel;
    lblDescription: TLabel;
    pnlStepOptions: TPanel;
    lblOptionsHeader: TLabel;
    sbOptions: TScrollBox;
    pnlStepTarget: TPanel;
    lblTargetHeader: TLabel;
    pnlTargetInputs: TPanel;
    lblTarget: TLabel;
    edtTargetDir: TEdit;
    btnBrowse: TButton;
    lblProj: TLabel;
    edtProj: TEdit;
    lblUnit: TLabel;
    edtUnit: TEdit;
    pnlSummary: TPanel;
    lblSummary: TLabel;
    memSummary: TMemo;
    pnlButtons: TPanel;
    btnBack: TButton;
    btnNext: TButton;
    btnFinish: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure tvStartersChange(Sender: TObject; Node: TTreeNode);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnFinishClick(Sender: TObject);
  strict private
    FSelected: TOBDStarter;
    FHasSelection: Boolean;
    FStep: TOBDStarterStep;
    FOptionControls: TList<TComponent>;
  public
    destructor Destroy; override;
  strict private
    procedure SelectStarter(const AStarter: TOBDStarter);
    procedure GoToStep(AStep: TOBDStarterStep);
    procedure UpdateNavigation;
    procedure BuildOptionsPage;
    procedure CollectOptions(out AChoices: TDictionary<string, TArray<string>>);
    procedure BuildSummary(const AChoices: TDictionary<string, TArray<string>>);
    function StarterHasOptions: Boolean;
    function StepAfter(ACurrent: TOBDStarterStep): TOBDStarterStep;
    function StepBefore(ACurrent: TOBDStarterStep): TOBDStarterStep;
  public
    /// <summary>Runs the wizard. On accept, populates
    /// <c>AContext</c> (including
    /// <see cref="TOBDStarterContext.Choices"/> — caller takes
    /// ownership of the dictionary) and <c>AStarter</c> and
    /// returns <c>True</c>.</summary>
    class function Pick(out AContext: TOBDStarterContext;
      out AStarter: TOBDStarter): Boolean; static;
  end;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  Vcl.FileCtrl;

{ ---- TOBDStarterPickerDlg --------------------------------------------------- }

destructor TOBDStarterPickerDlg.Destroy;
begin
  FOptionControls.Free;
  inherited;
end;

procedure TOBDStarterPickerDlg.FormCreate(Sender: TObject);
var
  All: TArray<TOBDStarter>;
  I: Integer;
  CategoryNode: TTreeNode;
  CurrentCategory: string;
begin
  FOptionControls := TList<TComponent>.Create;
  FStep := sssStarter;

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
      tvStarters.Items.AddChildObject(
        CategoryNode, All[I].Title, Pointer(NativeInt(I) + 1));
    end;
    tvStarters.FullExpand;
  finally
    tvStarters.Items.EndUpdate;
  end;

  edtProj.Text := 'MyDelphiOBDProject';
  edtUnit.Text := 'MainForm';
  edtTargetDir.Text := TPath.Combine(
    TPath.GetDocumentsPath, 'Delphi-OBD');

  GoToStep(sssStarter);
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
    UpdateNavigation;
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
  UpdateNavigation;
end;

function TOBDStarterPickerDlg.StarterHasOptions: Boolean;
begin
  Result := FHasSelection and (Length(FSelected.OptionGroups) > 0);
end;

function TOBDStarterPickerDlg.StepAfter(
  ACurrent: TOBDStarterStep): TOBDStarterStep;
begin
  case ACurrent of
    sssStarter:
      if StarterHasOptions then Result := sssOptions else Result := sssTarget;
    sssOptions: Result := sssTarget;
  else
    Result := ACurrent;
  end;
end;

function TOBDStarterPickerDlg.StepBefore(
  ACurrent: TOBDStarterStep): TOBDStarterStep;
begin
  case ACurrent of
    sssTarget:
      if StarterHasOptions then Result := sssOptions else Result := sssStarter;
    sssOptions: Result := sssStarter;
  else
    Result := ACurrent;
  end;
end;

procedure TOBDStarterPickerDlg.GoToStep(AStep: TOBDStarterStep);
var
  Choices: TDictionary<string, TArray<string>>;
begin
  FStep := AStep;
  pnlStepStarter.Visible := AStep = sssStarter;
  pnlStepOptions.Visible := AStep = sssOptions;
  pnlStepTarget.Visible  := AStep = sssTarget;
  case AStep of
    sssOptions:
      BuildOptionsPage;
    sssTarget:
      begin
        Choices := TDictionary<string, TArray<string>>.Create;
        try
          CollectOptions(Choices);
          BuildSummary(Choices);
        finally
          Choices.Free;
        end;
      end;
  end;
  UpdateNavigation;
end;

procedure TOBDStarterPickerDlg.UpdateNavigation;
begin
  btnCancel.Enabled := True;
  btnBack.Enabled   := FStep <> sssStarter;
  btnNext.Enabled   := FHasSelection and (FStep <> sssTarget);
  btnFinish.Enabled := FHasSelection and (FStep = sssTarget);
  btnNext.Default   := btnNext.Enabled;
  btnFinish.Default := btnFinish.Enabled;
end;

procedure TOBDStarterPickerDlg.BuildOptionsPage;
const
  GROUP_GAP = 16;
var
  C: TComponent;
  Y: Integer;
  G: Integer;
  Group: TOBDStarterOptionGroup;
  GroupBox: TGroupBox;
  CtrlTop: Integer;
  Help: TLabel;
  I: Integer;
  Box: TCheckBox;
  Radio: TRadioButton;
begin
  // Tear down the previous page.
  for C in FOptionControls do
    C.Free;
  FOptionControls.Clear;

  Y := 0;
  for G := 0 to High(FSelected.OptionGroups) do
  begin
    Group := FSelected.OptionGroups[G];
    GroupBox := TGroupBox.Create(sbOptions);
    GroupBox.Parent := sbOptions;
    GroupBox.Name := 'gb_' + Group.Id;
    GroupBox.Caption := ' ' + Group.Title + ' ';
    GroupBox.Tag := G;
    GroupBox.Top := Y;
    GroupBox.Left := 12;
    GroupBox.Width := sbOptions.ClientWidth - 28;
    GroupBox.Anchors := [akLeft, akTop, akRight];
    FOptionControls.Add(GroupBox);

    CtrlTop := 24;
    if Group.Description <> '' then
    begin
      Help := TLabel.Create(GroupBox);
      Help.Parent := GroupBox;
      Help.Top := CtrlTop;
      Help.Left := 12;
      Help.Width := GroupBox.Width - 24;
      Help.AutoSize := False;
      Help.WordWrap := True;
      Help.Caption := Group.Description;
      Help.Font.Color := clGrayText;
      Help.Height := 32;
      Inc(CtrlTop, Help.Height + 4);
    end;

    for I := 0 to High(Group.Options) do
    begin
      case Group.Kind of
        sokMultiSelect:
          begin
            Box := TCheckBox.Create(GroupBox);
            Box.Parent := GroupBox;
            Box.Tag := G * 1000 + I;
            Box.Top := CtrlTop;
            Box.Left := 16;
            Box.Width := GroupBox.Width - 32;
            Box.Caption := Group.Options[I].Caption;
            Box.Checked := Group.Options[I].Selected;
            Inc(CtrlTop, 24);
          end;
        sokSingleSelect:
          begin
            Radio := TRadioButton.Create(GroupBox);
            Radio.Parent := GroupBox;
            Radio.Tag := G * 1000 + I;
            Radio.Top := CtrlTop;
            Radio.Left := 16;
            Radio.Width := GroupBox.Width - 32;
            Radio.Caption := Group.Options[I].Caption;
            Radio.Checked := Group.Options[I].Selected;
            Inc(CtrlTop, 24);
          end;
      end;
    end;

    GroupBox.Height := CtrlTop + 12;
    Inc(Y, GroupBox.Height + GROUP_GAP);
  end;
end;

procedure TOBDStarterPickerDlg.CollectOptions(
  out AChoices: TDictionary<string, TArray<string>>);
var
  G: Integer;
  Group: TOBDStarterOptionGroup;
  GroupBox: TGroupBox;
  Selected: TList<string>;
  C: TComponent;
  I: Integer;
begin
  AChoices := TDictionary<string, TArray<string>>.Create;
  for G := 0 to High(FSelected.OptionGroups) do
  begin
    Group := FSelected.OptionGroups[G];
    Selected := TList<string>.Create;
    try
      // Find the GroupBox we built for this group (by Tag).
      GroupBox := nil;
      for C in FOptionControls do
        if (C is TGroupBox) and (C.Tag = G) then
        begin
          GroupBox := TGroupBox(C);
          Break;
        end;
      if GroupBox <> nil then
        for I := 0 to GroupBox.ControlCount - 1 do
        begin
          if (GroupBox.Controls[I] is TCheckBox) and
             TCheckBox(GroupBox.Controls[I]).Checked then
            Selected.Add(Group.Options[GroupBox.Controls[I].Tag mod 1000].Id);
          if (GroupBox.Controls[I] is TRadioButton) and
             TRadioButton(GroupBox.Controls[I]).Checked then
            Selected.Add(Group.Options[GroupBox.Controls[I].Tag mod 1000].Id);
        end;
      AChoices.Add(Group.Id, Selected.ToArray);
    finally
      Selected.Free;
    end;
  end;
end;

procedure TOBDStarterPickerDlg.BuildSummary(
  const AChoices: TDictionary<string, TArray<string>>);
var
  Lines: TStringList;
  G, J: Integer;
  Group: TOBDStarterOptionGroup;
  Selected: TArray<string>;
  S, Joined, OptCaption: string;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('Starter   : ' + FSelected.Title);
    Lines.Add('Category  : ' + FSelected.Category);
    if AChoices.Count > 0 then
    begin
      Lines.Add('');
      for G := 0 to High(FSelected.OptionGroups) do
      begin
        Group := FSelected.OptionGroups[G];
        if not AChoices.TryGetValue(Group.Id, Selected) then Continue;
        Joined := '';
        for S in Selected do
          for J := 0 to High(Group.Options) do
            if Group.Options[J].Id = S then
            begin
              OptCaption := Group.Options[J].Caption;
              if Joined = '' then
                Joined := OptCaption
              else
                Joined := Joined + ', ' + OptCaption;
            end;
        if Joined = '' then
          Lines.Add(Format('%-10s: (none)', [Group.Title]))
        else
          Lines.Add(Format('%-10s: %s', [Group.Title, Joined]));
      end;
    end;
    memSummary.Lines.Text := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TOBDStarterPickerDlg.btnBrowseClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := edtTargetDir.Text;
  if SelectDirectory('Select target folder', '', Dir) then
    edtTargetDir.Text := Dir;
end;

procedure TOBDStarterPickerDlg.btnBackClick(Sender: TObject);
begin
  GoToStep(StepBefore(FStep));
end;

procedure TOBDStarterPickerDlg.btnNextClick(Sender: TObject);
begin
  if not FHasSelection then Exit;
  GoToStep(StepAfter(FStep));
end;

procedure TOBDStarterPickerDlg.btnFinishClick(Sender: TObject);
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
  AContext := Default(TOBDStarterContext);
  Dlg := TOBDStarterPickerDlg.Create(nil);
  try
    if (Dlg.ShowModal = mrOk) and Dlg.FHasSelection then
    begin
      AStarter := Dlg.FSelected;
      ProjDir := TPath.Combine(Dlg.edtTargetDir.Text,
        Trim(Dlg.edtProj.Text));
      AContext.TargetDir        := ProjDir;
      AContext.ProjectName      := Trim(Dlg.edtProj.Text);
      AContext.UnitName         := Trim(Dlg.edtUnit.Text);
      AContext.FormClassName    := 'T' + AContext.UnitName;
      AContext.FormInstanceName := AContext.UnitName;
      Dlg.CollectOptions(AContext.Choices);
      // Caller takes ownership of AContext.Choices.
      Result := True;
    end;
  finally
    Dlg.Free;
  end;
end;

end.
