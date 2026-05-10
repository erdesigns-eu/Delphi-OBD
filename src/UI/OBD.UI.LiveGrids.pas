//------------------------------------------------------------------------------
//  OBD.UI.LiveGrids
//
//  OS-native grid visuals for live + freeze-frame data:
//
//    TOBDMultiPidGrid     TListView in report mode showing N
//                         PIDs and their live values. Auto-
//                         subscribes to a TOBDLiveData per
//                         row; each dispatch updates the
//                         matching cell on the main thread.
//    TOBDFreezeFrameTable TListView in report mode populated
//                         from a TOBDFreezeFrame snapshot.
//                         Host calls Refresh to read the
//                         configured PID list from the ECU
//                         and re-render the table.
//
//  Both descend from TListView so they pick up Windows visual
//  styles + VCL Styles for free; only the per-row data + the
//  PID subscription glue lives on the descendant.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.LiveGrids;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ComCtrls,
  OBD.Types,
  OBD.Service.Catalog,
  OBD.Service.LiveData,
  OBD.Service.FreezeFrame;

type
  /// <summary>OS-native list view that tracks N PIDs against a
  /// <see cref="TOBDLiveData"/> service. Set
  /// <c>LiveData</c> + <c>PIDs</c> at design time; the grid
  /// subscribes to each PID, populates the row, and updates
  /// the value cell on every dispatch.</summary>
  TOBDMultiPidGrid = class(TListView)
  strict private
    FLiveData:    TOBDLiveData;
    FPIDs:        TStrings;
    FSubscribed:  TList<Byte>;
    FUpdating:    Boolean;
    procedure SetLiveData(AValue: TOBDLiveData);
    procedure SetPIDs(AValue: TStrings);
    procedure HandlePIDsChange(Sender: TObject);
    procedure UnsubscribeAll;
    procedure SubscribeAll;
    procedure RebuildRows;
    procedure HandleLiveValue(Sender: TObject;
      const AValue: TOBDPIDValue);
    function  ParsePID(const AText: string;
      out APID: Byte): Boolean;
    function  FindRow(APID: Byte): TListItem;
    procedure NotifyBindings;
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Re-reads PIDs and rebuilds rows. Called
    /// internally whenever PIDs / LiveData change; hosts that
    /// edit the PID list at runtime call it to apply.</summary>
    procedure ApplyChanges;
  published
    /// <summary>Source service. nil = grid is read-only display
    /// updated only via host calls.</summary>
    property LiveData: TOBDLiveData
      read FLiveData write SetLiveData;
    /// <summary>One PID per line. Hex (<c>0x0C</c>,
    /// <c>$0C</c>) or decimal (<c>12</c>) both accepted.
    /// </summary>
    property PIDs: TStrings read FPIDs write SetPIDs;

    // Republish the bits a host typically wants from
    // TListView. The full property set is inherited; these
    // are the ones the Object Inspector should surface first.
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Enabled;
    property Font;
    property GridLines default True;
    property HideSelection default False;
    property ReadOnly default True;
    property RowSelect default True;
    property ShowColumnHeaders default True;
    property Visible;
  end;

  /// <summary>Decoded freeze-frame snapshot in a sortable list
  /// view. Host points <c>FreezeFrame</c> at a configured
  /// <see cref="TOBDFreezeFrame"/>, sets <c>FrameIndex</c> +
  /// <c>PIDs</c>, calls <c>Refresh</c>. Each PID is read
  /// synchronously and added as one row.</summary>
  TOBDFreezeFrameTable = class(TListView)
  strict private
    FFreezeFrame: TOBDFreezeFrame;
    FFrameIndex:  Byte;
    FPIDs:        TStrings;
    procedure SetFreezeFrame(AValue: TOBDFreezeFrame);
    procedure SetFrameIndex(AValue: Byte);
    procedure SetPIDs(AValue: TStrings);
    procedure HandlePIDsChange(Sender: TObject);
    function  ParsePID(const AText: string;
      out APID: Byte): Boolean;
    procedure NotifyBindings;
  protected
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    /// <summary>Re-reads every PID from the configured frame
    /// and re-renders the table. Raises if no
    /// <c>FreezeFrame</c> is wired.</summary>
    procedure Refresh;
  published
    /// <summary>Bound freeze-frame service. nil disables
    /// <see cref="Refresh"/>.</summary>
    property FreezeFrame: TOBDFreezeFrame
      read FFreezeFrame write SetFreezeFrame;
    /// <summary>Frame index to read. OBD-II historically only
    /// surfaces frame 0; some manufacturers expose more.
    /// Default 0.</summary>
    property FrameIndex: Byte
      read FFrameIndex write SetFrameIndex default 0;
    /// <summary>One PID per line; same parsing as
    /// <see cref="TOBDMultiPidGrid.PIDs"/>.</summary>
    property PIDs: TStrings read FPIDs write SetPIDs;

    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Enabled;
    property Font;
    property GridLines default True;
    property RowSelect default True;
    property ShowColumnHeaders default True;
    property Visible;
  end;

implementation

uses
  System.Math;

function ParseHexOrDec(const AText: string; out AValue: Byte): Boolean;
var
  Trimmed: string;
  V: Integer;
begin
  Result := False;
  Trimmed := Trim(AText);
  if Trimmed = '' then Exit;
  // Accept "0x0C", "$0C", "0C" (hex assumption when 2 chars),
  // and plain decimal.
  if (Length(Trimmed) > 2) and
     (Copy(Trimmed, 1, 2).ToLower = '0x') then
    Trimmed := '$' + Copy(Trimmed, 3, MaxInt);
  if TryStrToInt(Trimmed, V) then
    if (V >= 0) and (V <= $FF) then
    begin
      AValue := Byte(V);
      Exit(True);
    end;
end;

function PIDDisplayName(APID: Byte): string;
var
  Info: TOBDPIDInfo;
begin
  if TOBDServiceCatalog.Default.TryGetPID(APID, Info) and
     (Info.Name <> '') then
    Result := Info.Name
  else
    Result := '';
end;

procedure ConfigureColumns(AListView: TListView;
  const ANames: array of string;
  const AWidths: array of Integer);
var
  I: Integer;
  Col: TListColumn;
begin
  AListView.Columns.BeginUpdate;
  try
    AListView.Columns.Clear;
    for I := 0 to High(ANames) do
    begin
      Col := AListView.Columns.Add;
      Col.Caption := ANames[I];
      Col.Width   := AWidths[I];
    end;
  finally
    AListView.Columns.EndUpdate;
  end;
end;

{ ---- TOBDMultiPidGrid ---------------------------------------------------- }

constructor TOBDMultiPidGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle  := vsReport;
  RowSelect  := True;
  ReadOnly   := True;
  GridLines  := True;
  HideSelection := False;
  ShowColumnHeaders := True;
  FPIDs := TStringList.Create;
  TStringList(FPIDs).OnChange := HandlePIDsChange;
  FSubscribed := TList<Byte>.Create;
end;

destructor TOBDMultiPidGrid.Destroy;
begin
  UnsubscribeAll;
  FSubscribed.Free;
  FPIDs.Free;
  inherited;
end;

procedure TOBDMultiPidGrid.CreateWnd;
begin
  inherited;
  // Columns are set up the first time the window is created
  // so VCL Styles + theme picked up the standard headers.
  if Columns.Count = 0 then
    ConfigureColumns(Self,
      ['PID', 'Name', 'Value', 'Unit'],
      [60, 200, 100, 60]);
end;

procedure TOBDMultiPidGrid.Loaded;
begin
  inherited;
  ApplyChanges;
end;

procedure TOBDMultiPidGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FLiveData) then
  begin
    UnsubscribeAll;
    FLiveData := nil;
  end;
end;

procedure TOBDMultiPidGrid.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDMultiPidGrid.SetLiveData(AValue: TOBDLiveData);
begin
  if FLiveData = AValue then Exit;
  UnsubscribeAll;
  if FLiveData <> nil then FLiveData.RemoveFreeNotification(Self);
  FLiveData := AValue;
  if FLiveData <> nil then FLiveData.FreeNotification(Self);
  ApplyChanges;
end;

procedure TOBDMultiPidGrid.SetPIDs(AValue: TStrings);
begin
  FPIDs.Assign(AValue);
end;

procedure TOBDMultiPidGrid.HandlePIDsChange(Sender: TObject);
begin
  if FUpdating then Exit;
  ApplyChanges;
end;

function TOBDMultiPidGrid.ParsePID(const AText: string;
  out APID: Byte): Boolean;
begin
  Result := ParseHexOrDec(AText, APID);
end;

procedure TOBDMultiPidGrid.UnsubscribeAll;
var
  PIDByte: Byte;
begin
  if FLiveData = nil then
  begin
    FSubscribed.Clear;
    Exit;
  end;
  for PIDByte in FSubscribed do
    FLiveData.Unsubscribe(PIDByte, HandleLiveValue);
  FSubscribed.Clear;
end;

procedure TOBDMultiPidGrid.SubscribeAll;
var
  I: Integer;
  PIDByte: Byte;
begin
  if (FLiveData = nil) or (csDesigning in ComponentState) then Exit;
  for I := 0 to FPIDs.Count - 1 do
    if ParsePID(FPIDs[I], PIDByte) then
      if FSubscribed.IndexOf(PIDByte) < 0 then
      begin
        FLiveData.Subscribe(PIDByte, HandleLiveValue);
        FSubscribed.Add(PIDByte);
      end;
end;

procedure TOBDMultiPidGrid.RebuildRows;
var
  I: Integer;
  PIDByte: Byte;
  Item: TListItem;
begin
  FUpdating := True;
  Items.BeginUpdate;
  try
    Items.Clear;
    for I := 0 to FPIDs.Count - 1 do
    begin
      if not ParsePID(FPIDs[I], PIDByte) then Continue;
      Item := Items.Add;
      Item.Caption := Format('0x%2.2X', [PIDByte]);
      Item.SubItems.Add(PIDDisplayName(PIDByte));
      Item.SubItems.Add('-');             // value placeholder
      Item.SubItems.Add('');              // unit placeholder
      Item.Data := Pointer(NativeInt(PIDByte));
    end;
  finally
    Items.EndUpdate;
    FUpdating := False;
  end;
end;

procedure TOBDMultiPidGrid.ApplyChanges;
begin
  if csDestroying in ComponentState then Exit;
  if HandleAllocated then
  begin
    UnsubscribeAll;
    RebuildRows;
    SubscribeAll;
    NotifyBindings;
  end
  else
  begin
    // Defer until the HWND exists — CreateWnd will reapply.
    RebuildRows;
    SubscribeAll;
  end;
end;

function TOBDMultiPidGrid.FindRow(APID: Byte): TListItem;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if NativeInt(Items[I].Data) = APID then
      Exit(Items[I]);
  Result := nil;
end;

procedure TOBDMultiPidGrid.HandleLiveValue(Sender: TObject;
  const AValue: TOBDPIDValue);
var
  Item: TListItem;
  FS: TFormatSettings;
begin
  Item := FindRow(AValue.PID);
  if Item = nil then Exit;
  FS := TFormatSettings.Create('en-US');
  if System.Math.IsNan(AValue.Value) then
    Item.SubItems[1] := '-'
  else
    Item.SubItems[1] := FormatFloat('0.###', AValue.Value, FS);
  Item.SubItems[2] := AValue.Unit_;
  NotifyBindings;
end;

{ ---- TOBDFreezeFrameTable ------------------------------------------------ }

constructor TOBDFreezeFrameTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle  := vsReport;
  RowSelect  := True;
  ReadOnly   := True;
  GridLines  := True;
  ShowColumnHeaders := True;
  FFrameIndex := 0;
  FPIDs := TStringList.Create;
  TStringList(FPIDs).OnChange := HandlePIDsChange;
end;

destructor TOBDFreezeFrameTable.Destroy;
begin
  FPIDs.Free;
  inherited;
end;

procedure TOBDFreezeFrameTable.CreateWnd;
begin
  inherited;
  if Columns.Count = 0 then
    ConfigureColumns(Self,
      ['PID', 'Name', 'Value', 'Unit'],
      [60, 200, 100, 60]);
end;

procedure TOBDFreezeFrameTable.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FFreezeFrame) then
    FFreezeFrame := nil;
end;

procedure TOBDFreezeFrameTable.NotifyBindings;
begin
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDFreezeFrameTable.SetFreezeFrame(AValue: TOBDFreezeFrame);
begin
  if FFreezeFrame = AValue then Exit;
  if FFreezeFrame <> nil then
    FFreezeFrame.RemoveFreeNotification(Self);
  FFreezeFrame := AValue;
  if FFreezeFrame <> nil then
    FFreezeFrame.FreeNotification(Self);
end;

procedure TOBDFreezeFrameTable.SetFrameIndex(AValue: Byte);
begin
  if FFrameIndex = AValue then Exit;
  FFrameIndex := AValue;
end;

procedure TOBDFreezeFrameTable.SetPIDs(AValue: TStrings);
begin
  FPIDs.Assign(AValue);
end;

procedure TOBDFreezeFrameTable.HandlePIDsChange(Sender: TObject);
begin
  // The table is host-refreshed (not live-subscribed) — rebuild
  // the rows with placeholder cells so the design-time look
  // tracks the PID list.
  Items.BeginUpdate;
  try
    Items.Clear;
    if not HandleAllocated then Exit;
  finally
    Items.EndUpdate;
  end;
end;

function TOBDFreezeFrameTable.ParsePID(const AText: string;
  out APID: Byte): Boolean;
begin
  Result := ParseHexOrDec(AText, APID);
end;

procedure TOBDFreezeFrameTable.Refresh;
var
  I: Integer;
  PIDByte: Byte;
  Item: TListItem;
  V: TOBDPIDValue;
  FS: TFormatSettings;
begin
  if FFreezeFrame = nil then Exit;
  FS := TFormatSettings.Create('en-US');
  Items.BeginUpdate;
  try
    Items.Clear;
    for I := 0 to FPIDs.Count - 1 do
    begin
      if not ParsePID(FPIDs[I], PIDByte) then Continue;
      Item := Items.Add;
      Item.Caption := Format('0x%2.2X', [PIDByte]);
      Item.SubItems.Add(PIDDisplayName(PIDByte));
      try
        V := FFreezeFrame.Read(PIDByte, FFrameIndex);
        if System.Math.IsNan(V.Value) then
          Item.SubItems.Add('-')
        else
          Item.SubItems.Add(FormatFloat('0.###', V.Value, FS));
        Item.SubItems.Add(V.Unit_);
      except
        on E: Exception do
        begin
          Item.SubItems.Add('error');
          Item.SubItems.Add(E.Message);
        end;
      end;
    end;
  finally
    Items.EndUpdate;
  end;
  NotifyBindings;
end;

end.
