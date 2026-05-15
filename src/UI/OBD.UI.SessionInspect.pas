//------------------------------------------------------------------------------
//  OBD.UI.SessionInspect
//
//  Session / transport inspectors:
//
//    TOBDKWP1281SessionInspector  Counter / block count / kw1
//                                 / kw2 / last-block-title.
//    TOBDJ2534DeviceList          TListView enumerating
//                                 PassThru devices.
//    TOBDTP20ChannelPanel         TP2.0 channel state panel.
//    TOBDDoIPNodePicker           DoIP vehicle-identification
//                                 node picker.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.SessionInspect;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ComCtrls,
  OBD.UI.Types,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>KWP1281 session counters.</summary>
  TOBDKWP1281SessionInspector = class(TOBDCustomControl)
  strict private
    FCounter:        Byte;
    FBlockCount:     Cardinal;
    FKW1:            Byte;
    FKW2:            Byte;
    FLastBlockTitle: string;
    FFont:           TFont;
    procedure SetCounter(AValue: Byte);
    procedure SetBlockCount(AValue: Cardinal);
    procedure SetKW1(AValue: Byte);
    procedure SetKW2(AValue: Byte);
    procedure SetLastBlockTitle(const AValue: string);
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>KWP1281 block counter (0..255). Default 0.
    /// </summary>
    property Counter: Byte read FCounter write SetCounter default 0;
    /// <summary>Total blocks exchanged this session. Default 0.
    /// </summary>
    property BlockCount: Cardinal
      read FBlockCount write SetBlockCount default 0;
    /// <summary>First keyword byte from the init sequence.
    /// Default 0.</summary>
    property KW1: Byte read FKW1 write SetKW1 default 0;
    /// <summary>Second keyword byte from the init sequence.
    /// Default 0.</summary>
    property KW2: Byte read FKW2 write SetKW2 default 0;
    /// <summary>Human-readable title of the most recent block.
    /// </summary>
    property LastBlockTitle: string
      read FLastBlockTitle write SetLastBlockTitle;
    /// <summary>Monospaced font used for the counters readout.
    /// </summary>
    property MonoFont: TFont read FFont write SetFontA;
  end;

  /// <summary>One enumerated J2534 device.</summary>
  TOBDJ2534DeviceInfo = record
    Name:     string;
    Vendor:   string;
    DLL:      string;
    Protocols: string;
  end;

  /// <summary>TListView of installed PassThru devices. Host
  /// pushes the enumeration result via
  /// <see cref="LoadDevices"/>; the list is read-only.
  /// </summary>
  TOBDJ2534DeviceList = class(TListView)
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Replace the displayed rows with the supplied
    /// device list.</summary>
    procedure   LoadDevices(
      const ADevices: TArray<TOBDJ2534DeviceInfo>);
    /// <summary>Remove all rows.</summary>
    procedure   ClearDevices;
  protected
    procedure CreateWnd; override;
  published
    property Align;
    property Anchors;
    property Color;
    property Enabled;
    property Font;
    property GridLines default True;
    property RowSelect default True;
    property ShowColumnHeaders default True;
    property Visible;
  end;

  /// <summary>TP2.0 channel state panel.</summary>
  TOBDTP20ChannelPanel = class(TOBDCustomControl)
  strict private
    FRxId:          Word;
    FTxId:          Word;
    FKeepAliveMs:   Integer;
    FLastSendMs:    Int64;
    FFont:          TFont;
    procedure SetRxId(AValue: Word);
    procedure SetTxId(AValue: Word);
    procedure SetKeepAliveMs(AValue: Integer);
    procedure SetLastSendMs(AValue: Int64);
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Rx CAN identifier. Default 0.</summary>
    property RxId: Word read FRxId write SetRxId default 0;
    /// <summary>Tx CAN identifier. Default 0.</summary>
    property TxId: Word read FTxId write SetTxId default 0;
    /// <summary>Channel keep-alive interval in milliseconds.
    /// Default 0 (disabled).</summary>
    property KeepAliveMs: Integer
      read FKeepAliveMs write SetKeepAliveMs default 0;
    /// <summary>Milliseconds since the last successful send.
    /// Hosts update this from a tick handler.</summary>
    property LastSendMs: Int64
      read FLastSendMs write SetLastSendMs default 0;
    /// <summary>Monospaced font used for the channel readout.
    /// </summary>
    property MonoFont: TFont read FFont write SetFontA;
  end;

  /// <summary>One enumerated DoIP node from a vehicle-
  /// identification broadcast.</summary>
  TOBDDoIPNodeInfo = record
    EID:       Word;
    GID:       Word;
    VIN:       string;
    Hostname:  string;
    Address:   string;
  end;

  /// <summary>DoIP node picker. Host calls
  /// <see cref="LoadNodes"/> with the broadcast result; the
  /// list is read-only with double-click select via standard
  /// TListView events.</summary>
  TOBDDoIPNodePicker = class(TListView)
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Replace the displayed rows with the supplied
    /// node list.</summary>
    procedure   LoadNodes(const ANodes: TArray<TOBDDoIPNodeInfo>);
    /// <summary>Remove all rows.</summary>
    procedure   ClearNodes;
  protected
    procedure CreateWnd; override;
  published
    property Align;
    property Anchors;
    property Color;
    property Enabled;
    property Font;
    property GridLines default True;
    property RowSelect default True;
    property ShowColumnHeaders default True;
    property Visible;
    property OnDblClick;
  end;

implementation

procedure ConfigureCols(AListView: TListView;
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
      Col.Width := AWidths[I];
    end;
  finally
    AListView.Columns.EndUpdate;
  end;
end;

{ ---- TOBDKWP1281SessionInspector --------------------------------- }

constructor TOBDKWP1281SessionInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 110;
  FFont := TFont.Create;
  FFont.Name := 'Consolas';
  FFont.Size := 10;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDKWP1281SessionInspector.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDKWP1281SessionInspector.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDKWP1281SessionInspector.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDKWP1281SessionInspector.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDKWP1281SessionInspector.SetCounter(AValue: Byte);
begin
  if FCounter = AValue then Exit;
  FCounter := AValue; NotifyBindings; Repaint;
end;

procedure TOBDKWP1281SessionInspector.SetBlockCount(AValue: Cardinal);
begin
  if FBlockCount = AValue then Exit;
  FBlockCount := AValue; NotifyBindings; Repaint;
end;

procedure TOBDKWP1281SessionInspector.SetKW1(AValue: Byte);
begin
  if FKW1 = AValue then Exit;
  FKW1 := AValue; NotifyBindings; Repaint;
end;

procedure TOBDKWP1281SessionInspector.SetKW2(AValue: Byte);
begin
  if FKW2 = AValue then Exit;
  FKW2 := AValue; NotifyBindings; Repaint;
end;

procedure TOBDKWP1281SessionInspector.SetLastBlockTitle(
  const AValue: string);
begin
  if FLastBlockTitle = AValue then Exit;
  FLastBlockTitle := AValue; NotifyBindings; Repaint;
end;

procedure TOBDKWP1281SessionInspector.PaintControl(ACanvas: TCanvas);
var
  Pad, Y, RowH: Integer;

  procedure Row(const ALabel, AValue: string);
  begin
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, Y, ALabel);
    ACanvas.Font.Color := EffectiveAccent;
    ACanvas.TextOut(Pad + ScaleValue(120), Y, AValue);
    Inc(Y, RowH);
  end;

begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  Pad := ScaleValue(8);
  RowH := ACanvas.TextHeight('Mg') + ScaleValue(2);
  Y := Pad;
  Row('counter',         IntToStr(FCounter));
  Row('block count',     IntToStr(FBlockCount));
  Row('kw1 / kw2',       Format('0x%2.2X / 0x%2.2X', [FKW1, FKW2]));
  Row('last block',      FLastBlockTitle);
end;

{ ---- TOBDJ2534DeviceList ---------------------------------------- }

constructor TOBDJ2534DeviceList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  RowSelect := True;
  ReadOnly  := True;
  GridLines := True;
  ShowColumnHeaders := True;
end;

procedure TOBDJ2534DeviceList.CreateWnd;
begin
  inherited;
  if Columns.Count = 0 then
    ConfigureCols(Self,
      ['Name', 'Vendor', 'DLL', 'Protocols'],
      [180, 120, 240, 120]);
end;

procedure TOBDJ2534DeviceList.LoadDevices(
  const ADevices: TArray<TOBDJ2534DeviceInfo>);
var
  D: TOBDJ2534DeviceInfo;
  Item: TListItem;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for D in ADevices do
    begin
      Item := Items.Add;
      Item.Caption := D.Name;
      Item.SubItems.Add(D.Vendor);
      Item.SubItems.Add(D.DLL);
      Item.SubItems.Add(D.Protocols);
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TOBDJ2534DeviceList.ClearDevices;
begin
  Items.Clear;
end;

{ ---- TOBDTP20ChannelPanel --------------------------------------- }

constructor TOBDTP20ChannelPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 320;
  Height := 110;
  FFont := TFont.Create;
  FFont.Name := 'Consolas';
  FFont.Size := 10;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDTP20ChannelPanel.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDTP20ChannelPanel.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDTP20ChannelPanel.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDTP20ChannelPanel.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TOBDTP20ChannelPanel.SetRxId(AValue: Word);
begin
  if FRxId = AValue then Exit;
  FRxId := AValue; NotifyBindings; Repaint;
end;

procedure TOBDTP20ChannelPanel.SetTxId(AValue: Word);
begin
  if FTxId = AValue then Exit;
  FTxId := AValue; NotifyBindings; Repaint;
end;

procedure TOBDTP20ChannelPanel.SetKeepAliveMs(AValue: Integer);
begin
  if FKeepAliveMs = AValue then Exit;
  FKeepAliveMs := AValue; NotifyBindings; Repaint;
end;

procedure TOBDTP20ChannelPanel.SetLastSendMs(AValue: Int64);
begin
  if FLastSendMs = AValue then Exit;
  FLastSendMs := AValue; Repaint;
end;

procedure TOBDTP20ChannelPanel.PaintControl(ACanvas: TCanvas);
var
  Pad, Y, RowH: Integer;
  KeepCol: TColor;

  procedure Row(const ALabel, AValue: string; AColor: TColor);
  begin
    ACanvas.Font.Color := EffectiveForeground;
    ACanvas.TextOut(Pad, Y, ALabel);
    ACanvas.Font.Color := AColor;
    ACanvas.TextOut(Pad + ScaleValue(120), Y, AValue);
    Inc(Y, RowH);
  end;

begin
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  Pad := ScaleValue(8);
  RowH := ACanvas.TextHeight('Mg') + ScaleValue(2);
  Y := Pad;
  Row('rx id',  Format('0x%4.4X', [FRxId]),  EffectiveAccent);
  Row('tx id',  Format('0x%4.4X', [FTxId]),  EffectiveAccent);
  if (FKeepAliveMs > 0) and (FLastSendMs > FKeepAliveMs) then
    KeepCol := Palette.Danger
  else
    KeepCol := EffectiveAccent;
  Row('keep alive', Format('%d ms', [FKeepAliveMs]), KeepCol);
  Row('last send',  Format('%d ms ago', [FLastSendMs]),
    KeepCol);
end;

{ ---- TOBDDoIPNodePicker ---------------------------------------- }

constructor TOBDDoIPNodePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  RowSelect := True;
  ReadOnly  := True;
  GridLines := True;
  ShowColumnHeaders := True;
end;

procedure TOBDDoIPNodePicker.CreateWnd;
begin
  inherited;
  if Columns.Count = 0 then
    ConfigureCols(Self,
      ['EID', 'GID', 'VIN', 'Host', 'Address'],
      [60, 60, 200, 140, 120]);
end;

procedure TOBDDoIPNodePicker.LoadNodes(
  const ANodes: TArray<TOBDDoIPNodeInfo>);
var
  N: TOBDDoIPNodeInfo;
  Item: TListItem;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for N in ANodes do
    begin
      Item := Items.Add;
      Item.Caption := Format('0x%4.4X', [N.EID]);
      Item.SubItems.Add(Format('0x%4.4X', [N.GID]));
      Item.SubItems.Add(N.VIN);
      Item.SubItems.Add(N.Hostname);
      Item.SubItems.Add(N.Address);
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TOBDDoIPNodePicker.ClearNodes;
begin
  Items.Clear;
end;

end.
