//------------------------------------------------------------------------------
// UNIT           : CatalogBrowser.MainForm
// CONTENTS       : Programmatic VCL main form for the catalog
//                  browser example.
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Layout: TTreeView on the left listing every
//                  shipped catalog and its top-level sections;
//                  TListView upper-right showing the selected
//                  section's entries; TMemo lower-right showing
//                  the selected entry's full detail.
//------------------------------------------------------------------------------
unit CatalogBrowser.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.IOUtils, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  OBD.OEM.Catalog.JSON;

type
  TCatalogSection = (
    csEcus, csDids, csRoutines, csCodingBlocks, csAdaptations,
    csActuatorTests, csLivePIDs, csDtcExtended
  );

  /// <summary>One loaded catalog + its origin path.</summary>
  TLoadedCatalog = class
  public
    Catalog: TOBDOEMJSONCatalog;
    Path: string;
    destructor Destroy; override;
  end;

  TCatalogBrowserForm = class(TForm)
  strict private
    FPnlTop: TPanel;
    FBtnReload: TButton;
    FLblStatus: TLabel;
    FSplitter: TSplitter;
    FTree: TTreeView;
    FRightPanel: TPanel;
    FList: TListView;
    FListSplitter: TSplitter;
    FMemo: TMemo;
    FCatalogs: TObjectList<TLoadedCatalog>;

    procedure BuildLayout;
    procedure ReloadCatalogs(Sender: TObject);
    function  CatalogsDirectory: string;
    procedure WalkCatalogsInto(const Dir: string; List: TStringList);
    procedure PopulateTree;
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure ListSelect(Sender: TObject; Item: TListItem;
                         Selected: Boolean);
    procedure ShowEntries(const Cat: TOBDOEMJSONCatalog;
                          Section: TCatalogSection);
    function  EntryDetail(const Cat: TOBDOEMJSONCatalog;
                          Section: TCatalogSection;
                          Index: Integer): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  CatalogBrowserForm: TCatalogBrowserForm;

implementation

destructor TLoadedCatalog.Destroy;
begin
  Catalog.Free;
  inherited;
end;

constructor TCatalogBrowserForm.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Delphi-OBD Catalog Browser';
  Position := poScreenCenter;
  Width := 1180;
  Height := 760;
  FCatalogs := TObjectList<TLoadedCatalog>.Create(True);
  BuildLayout;
  ReloadCatalogs(nil);
end;

destructor TCatalogBrowserForm.Destroy;
begin
  FCatalogs.Free;
  inherited;
end;

procedure TCatalogBrowserForm.BuildLayout;
begin
  // Top toolbar
  FPnlTop := TPanel.Create(Self);
  FPnlTop.Parent := Self;
  FPnlTop.Align := alTop;
  FPnlTop.Height := 36;
  FPnlTop.BevelOuter := bvNone;

  FBtnReload := TButton.Create(Self);
  FBtnReload.Parent := FPnlTop;
  FBtnReload.SetBounds(8, 6, 110, 24);
  FBtnReload.Caption := 'Reload catalogs';
  FBtnReload.OnClick := ReloadCatalogs;

  FLblStatus := TLabel.Create(Self);
  FLblStatus.Parent := FPnlTop;
  FLblStatus.SetBounds(132, 10, 800, 16);
  FLblStatus.Caption := '';

  // Tree (left)
  FTree := TTreeView.Create(Self);
  FTree.Parent := Self;
  FTree.Align := alLeft;
  FTree.Width := 320;
  FTree.ReadOnly := True;
  FTree.HideSelection := False;
  FTree.OnChange := TreeChange;

  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Align := alLeft;
  FSplitter.Width := 4;

  // Right panel hosts list + memo
  FRightPanel := TPanel.Create(Self);
  FRightPanel.Parent := Self;
  FRightPanel.Align := alClient;
  FRightPanel.BevelOuter := bvNone;

  FList := TListView.Create(Self);
  FList.Parent := FRightPanel;
  FList.Align := alClient;
  FList.ViewStyle := vsReport;
  FList.ReadOnly := True;
  FList.RowSelect := True;
  FList.HideSelection := False;
  FList.OnSelectItem := ListSelect;

  FListSplitter := TSplitter.Create(Self);
  FListSplitter.Parent := FRightPanel;
  FListSplitter.Align := alBottom;
  FListSplitter.Height := 4;

  FMemo := TMemo.Create(Self);
  FMemo.Parent := FRightPanel;
  FMemo.Align := alBottom;
  FMemo.Height := 220;
  FMemo.ReadOnly := True;
  FMemo.ScrollBars := ssBoth;
  FMemo.Font.Name := 'Consolas';
  FMemo.Font.Size := 9;
end;

function TCatalogBrowserForm.CatalogsDirectory: string;
var
  Dir: string;
begin
  // Walk up from the example binary looking for a `catalogs` folder.
  Dir := ExtractFilePath(ParamStr(0));
  while (Dir <> '') and (not TDirectory.Exists(TPath.Combine(Dir, 'catalogs'))) do
  begin
    if TPath.IsPathRooted(Dir) and (Dir = ExtractFilePath(ExcludeTrailingPathDelimiter(Dir))) then
      Break;
    Dir := ExtractFilePath(ExcludeTrailingPathDelimiter(Dir));
  end;
  Result := TPath.Combine(Dir, 'catalogs');
end;

procedure TCatalogBrowserForm.WalkCatalogsInto(const Dir: string;
  List: TStringList);
var
  Files: TArray<string>;
  F, Lower: string;
begin
  if not TDirectory.Exists(Dir) then Exit;
  Files := TDirectory.GetFiles(Dir, '*.json', TSearchOption.soAllDirectories);
  for F in Files do
  begin
    Lower := LowerCase(ExtractFileName(F));
    // Skip schema, DTC catalogs, OBD2/UDS/ISO universal catalogs and
    // test fixtures; only show OEM-specific catalogs.
    if (Pos('schema', Lower) > 0) or (Pos('dtc-', Lower) = 1) or
       (Pos('obd2-', Lower) = 1) or (Pos('uds-', Lower) = 1) or
       (Pos('iso-', Lower) = 1) or (Pos('test-', Lower) = 1) then
      Continue;
    List.Add(F);
  end;
end;

procedure TCatalogBrowserForm.ReloadCatalogs(Sender: TObject);
var
  Dir: string;
  Files: TStringList;
  F: string;
  Loaded: TLoadedCatalog;
  Errors: Integer;
begin
  FCatalogs.Clear;
  FTree.Items.Clear;
  FList.Clear;
  FMemo.Clear;

  Dir := CatalogsDirectory;
  Files := TStringList.Create;
  try
    Files.Sorted := True;
    WalkCatalogsInto(Dir, Files);
    Errors := 0;
    for F in Files do
    begin
      Loaded := TLoadedCatalog.Create;
      try
        Loaded.Path := F;
        Loaded.Catalog := TOBDOEMJSONCatalog.Create(F);
        FCatalogs.Add(Loaded);
      except
        on E: Exception do
        begin
          Loaded.Free;
          Inc(Errors);
        end;
      end;
    end;
    FLblStatus.Caption := Format('%d catalogs loaded from %s (%d failed)',
      [FCatalogs.Count, Dir, Errors]);
  finally
    Files.Free;
  end;
  PopulateTree;
end;

procedure TCatalogBrowserForm.PopulateTree;
var
  Loaded: TLoadedCatalog;
  Cat: TOBDOEMJSONCatalog;
  Root, Section: TTreeNode;

  procedure AddSection(const Caption: string; Count: Integer; S: TCatalogSection);
  begin
    Section := FTree.Items.AddChild(Root,
      Format('%s (%d)', [Caption, Count]));
    Section.Data := Pointer(NativeInt(Ord(S)) + (NativeInt(Loaded) shl 8));
    // Encode (catalog* , section enum) into Data; decode in TreeChange
  end;

begin
  FTree.Items.BeginUpdate;
  try
    FTree.Items.Clear;
    for Loaded in FCatalogs do
    begin
      Cat := Loaded.Catalog;
      Root := FTree.Items.Add(nil,
        Format('%s — %s', [Cat.ManufacturerKey, Cat.DisplayName]));
      Root.Data := Loaded;
      AddSection('ECUs',          Cat.ECUCount,         csEcus);
      AddSection('DIDs',          Cat.DIDCount,         csDids);
      AddSection('Routines',      Cat.RoutineCount,     csRoutines);
      AddSection('Coding blocks', Cat.CodingBlockCount, csCodingBlocks);
      AddSection('Adaptations',   Cat.AdaptationCount,  csAdaptations);
      AddSection('Actuator tests',Cat.ActuatorTestCount,csActuatorTests);
      AddSection('Live PIDs',     Cat.LivePIDCount,     csLivePIDs);
      AddSection('DTC extended',  Cat.DtcExtendedCount, csDtcExtended);
    end;
  finally
    FTree.Items.EndUpdate;
  end;
end;

procedure TCatalogBrowserForm.TreeChange(Sender: TObject; Node: TTreeNode);
var
  Encoded: NativeInt;
  Section: TCatalogSection;
  Loaded: TLoadedCatalog;
begin
  FList.Clear;
  FMemo.Clear;
  if (Node = nil) or (Node.Parent = nil) then Exit;
  Encoded := NativeInt(Node.Data);
  Section := TCatalogSection(Encoded and $FF);
  Loaded := TLoadedCatalog(Encoded shr 8);
  ShowEntries(Loaded.Catalog, Section);
end;

procedure TCatalogBrowserForm.ShowEntries(const Cat: TOBDOEMJSONCatalog;
  Section: TCatalogSection);
var
  I: Integer;
  Item: TListItem;
begin
  FList.Items.BeginUpdate;
  try
    FList.Items.Clear;
    FList.Columns.Clear;

    case Section of
      csEcus:
        begin
          FList.Columns.Add.Caption := 'Address';
          FList.Columns.Add.Caption := 'Name';
          FList.Columns.Add.Caption := 'Common name';
          FList.Columns[0].Width := 90;
          FList.Columns[1].Width := 180;
          FList.Columns[2].Width := 320;
          for I := 0 to Cat.ECUCount - 1 do
          begin
            Item := FList.Items.Add;
            Item.Caption := Format('0x%.4X', [Cat.ECU(I).Address]);
            Item.SubItems.Add(Cat.ECU(I).Name);
            Item.SubItems.Add(Cat.ECU(I).CommonName);
            Item.Data := Pointer(I);
          end;
        end;
      csDids:
        begin
          FList.Columns.Add.Caption := 'DID';
          FList.Columns.Add.Caption := 'Name';
          FList.Columns.Add.Caption := 'Description';
          FList.Columns[0].Width := 80;
          FList.Columns[1].Width := 220;
          FList.Columns[2].Width := 400;
          for I := 0 to Cat.DIDCount - 1 do
          begin
            Item := FList.Items.Add;
            Item.Caption := Format('0x%.4X', [Cat.DID(I).DID]);
            Item.SubItems.Add(Cat.DID(I).Name);
            Item.SubItems.Add(Cat.DID(I).Description);
            Item.Data := Pointer(I);
          end;
        end;
      csRoutines:
        begin
          FList.Columns.Add.Caption := 'ID';
          FList.Columns.Add.Caption := 'Name';
          FList.Columns.Add.Caption := 'Description';
          FList.Columns[0].Width := 80;
          FList.Columns[1].Width := 220;
          FList.Columns[2].Width := 400;
          for I := 0 to Cat.RoutineCount - 1 do
          begin
            Item := FList.Items.Add;
            Item.Caption := Format('0x%.4X', [Cat.Routine(I).Identifier]);
            Item.SubItems.Add(Cat.Routine(I).Name);
            Item.SubItems.Add(Cat.Routine(I).Description);
            Item.Data := Pointer(I);
          end;
        end;
      csCodingBlocks:
        begin
          FList.Columns.Add.Caption := 'DID';
          FList.Columns.Add.Caption := 'Name';
          FList.Columns.Add.Caption := 'Fields';
          FList.Columns[0].Width := 80;
          FList.Columns[1].Width := 240;
          FList.Columns[2].Width := 80;
          for I := 0 to Cat.CodingBlockCount - 1 do
          begin
            Item := FList.Items.Add;
            Item.Caption := Format('0x%.4X', [Cat.CodingBlock(I).DataIdentifier]);
            Item.SubItems.Add(Cat.CodingBlock(I).Name);
            Item.SubItems.Add(IntToStr(Length(Cat.CodingBlock(I).Fields)));
            Item.Data := Pointer(I);
          end;
        end;
      csAdaptations:
        begin
          FList.Columns.Add.Caption := 'Channel';
          FList.Columns.Add.Caption := 'Name';
          FList.Columns.Add.Caption := 'Description';
          FList.Columns[0].Width := 80;
          FList.Columns[1].Width := 220;
          FList.Columns[2].Width := 400;
          for I := 0 to Cat.AdaptationCount - 1 do
          begin
            Item := FList.Items.Add;
            Item.Caption := Format('0x%.4X', [Cat.Adaptation(I).Channel]);
            Item.SubItems.Add(Cat.Adaptation(I).Name);
            Item.SubItems.Add(Cat.Adaptation(I).Description);
            Item.Data := Pointer(I);
          end;
        end;
      csActuatorTests:
        begin
          FList.Columns.Add.Caption := 'ID';
          FList.Columns.Add.Caption := 'Name';
          FList.Columns.Add.Caption := 'Description';
          FList.Columns[0].Width := 80;
          FList.Columns[1].Width := 220;
          FList.Columns[2].Width := 400;
          for I := 0 to Cat.ActuatorTestCount - 1 do
          begin
            Item := FList.Items.Add;
            Item.Caption := Format('0x%.4X', [Cat.ActuatorTest(I).Identifier]);
            Item.SubItems.Add(Cat.ActuatorTest(I).Name);
            Item.SubItems.Add(Cat.ActuatorTest(I).Description);
            Item.Data := Pointer(I);
          end;
        end;
      csLivePIDs:
        begin
          FList.Columns.Add.Caption := 'PID';
          FList.Columns.Add.Caption := 'Name';
          FList.Columns.Add.Caption := 'Description';
          FList.Columns[0].Width := 80;
          FList.Columns[1].Width := 220;
          FList.Columns[2].Width := 400;
          for I := 0 to Cat.LivePIDCount - 1 do
          begin
            Item := FList.Items.Add;
            Item.Caption := Format('0x%.4X', [Cat.LivePID(I).PID]);
            Item.SubItems.Add(Cat.LivePID(I).Name);
            Item.SubItems.Add(Cat.LivePID(I).Description);
            Item.Data := Pointer(I);
          end;
        end;
      csDtcExtended:
        begin
          FList.Columns.Add.Caption := 'Code';
          FList.Columns.Add.Caption := 'Record #';
          FList.Columns.Add.Caption := 'Kind';
          FList.Columns.Add.Caption := 'Description';
          FList.Columns[0].Width := 100;
          FList.Columns[1].Width := 80;
          FList.Columns[2].Width := 140;
          FList.Columns[3].Width := 360;
          for I := 0 to Cat.DtcExtendedCount - 1 do
          begin
            Item := FList.Items.Add;
            Item.Caption := Cat.DtcExtended(I).DtcCode;
            Item.SubItems.Add(IntToStr(Cat.DtcExtended(I).RecordNumber));
            Item.SubItems.Add(Cat.DtcExtended(I).KindStr);
            Item.SubItems.Add(Cat.DtcExtended(I).Description);
            Item.Data := Pointer(I);
          end;
        end;
    end;
  finally
    FList.Items.EndUpdate;
  end;
end;

procedure TCatalogBrowserForm.ListSelect(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Encoded: NativeInt;
  Section: TCatalogSection;
  Loaded: TLoadedCatalog;
  Idx: Integer;
begin
  if (Item = nil) or (not Selected) then Exit;
  if FTree.Selected = nil then Exit;
  Encoded := NativeInt(FTree.Selected.Data);
  Section := TCatalogSection(Encoded and $FF);
  Loaded := TLoadedCatalog(Encoded shr 8);
  Idx := Integer(Item.Data);
  FMemo.Lines.Text := EntryDetail(Loaded.Catalog, Section, Idx);
end;

function TCatalogBrowserForm.EntryDetail(const Cat: TOBDOEMJSONCatalog;
  Section: TCatalogSection; Index: Integer): string;
var
  SB: TStringBuilder;
  I: Integer;
begin
  SB := TStringBuilder.Create;
  try
    case Section of
      csEcus:
        with Cat.ECU(Index) do
        begin
          SB.AppendLine(Format('Address     : 0x%.4X', [Address]));
          SB.AppendLine('Name        : ' + Name);
          SB.AppendLine('Common name : ' + CommonName);
        end;
      csDids:
        with Cat.DID(Index) do
        begin
          SB.AppendLine(Format('DID         : 0x%.4X', [DID]));
          SB.AppendLine('Name        : ' + Name);
          SB.AppendLine('Description : ' + Description);
          SB.AppendLine(Format('ECU address : 0x%.4X', [EcuAddress]));
          SB.AppendLine(Format('Verified    : %s', [BoolToStr(Verified, True)]));
          if Decoder.Unit_ <> '' then
            SB.AppendLine('Unit        : ' + Decoder.Unit_);
          if Decoder.Size > 0 then
            SB.AppendLine(Format('Size        : %d', [Decoder.Size]));
        end;
      csRoutines:
        with Cat.Routine(Index) do
        begin
          SB.AppendLine(Format('ID          : 0x%.4X', [Identifier]));
          SB.AppendLine('Name        : ' + Name);
          SB.AppendLine('Description : ' + Description);
          SB.AppendLine(Format('ECU address : 0x%.4X', [EcuAddress]));
        end;
      csCodingBlocks:
        with Cat.CodingBlock(Index) do
        begin
          SB.AppendLine(Format('DID         : 0x%.4X', [DataIdentifier]));
          SB.AppendLine('Name        : ' + Name);
          SB.AppendLine('Description : ' + Description);
          SB.AppendLine(Format('ECU address : 0x%.4X', [EcuAddress]));
          SB.AppendLine(Format('Payload size: %d bytes', [PayloadSize]));
          SB.AppendLine('Fields:');
          for I := 0 to High(Fields) do
            SB.AppendLine(Format('  %-30s @byte %d bit %d  width %d  (%s)',
              [Fields[I].Name, Fields[I].ByteOffset, Fields[I].BitOffset,
               Fields[I].BitWidth, Fields[I].KindStr]));
        end;
      csAdaptations:
        with Cat.Adaptation(Index) do
        begin
          SB.AppendLine(Format('Channel     : 0x%.4X', [Channel]));
          SB.AppendLine('Name        : ' + Name);
          SB.AppendLine('Description : ' + Description);
          SB.AppendLine(Format('ECU address : 0x%.4X', [EcuAddress]));
          SB.AppendLine(Format('Min / Max   : %d / %d', [MinValue, MaxValue]));
          SB.AppendLine(Format('Default     : %d', [DefaultValue]));
          if Unit_ <> '' then
            SB.AppendLine('Unit        : ' + Unit_);
        end;
      csActuatorTests:
        with Cat.ActuatorTest(Index) do
        begin
          SB.AppendLine(Format('ID          : 0x%.4X', [Identifier]));
          SB.AppendLine('Name        : ' + Name);
          SB.AppendLine('Description : ' + Description);
          SB.AppendLine(Format('ECU address : 0x%.4X', [EcuAddress]));
          SB.AppendLine(Format('Duration    : %d ms', [DurationMs]));
          if SafetyWarning <> '' then
            SB.AppendLine('Safety      : ' + SafetyWarning);
        end;
      csLivePIDs:
        with Cat.LivePID(Index) do
        begin
          SB.AppendLine(Format('PID         : 0x%.4X', [PID]));
          SB.AppendLine('Name        : ' + Name);
          SB.AppendLine('Description : ' + Description);
          SB.AppendLine('Mode        : ' + Mode);
          SB.AppendLine(Format('Frame offset: %d', [FrameOffset]));
          if Unit_ <> '' then
            SB.AppendLine('Unit        : ' + Unit_);
        end;
      csDtcExtended:
        with Cat.DtcExtended(Index) do
        begin
          SB.AppendLine('Code        : ' + DtcCode);
          SB.AppendLine(Format('Record #    : %d', [RecordNumber]));
          SB.AppendLine('Kind        : ' + KindStr);
          SB.AppendLine('Description : ' + Description);
          if Unit_ <> '' then
            SB.AppendLine('Unit        : ' + Unit_);
        end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

end.
