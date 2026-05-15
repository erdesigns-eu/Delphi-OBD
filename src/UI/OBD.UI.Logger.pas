//------------------------------------------------------------------------------
//  OBD.UI.Logger
//
//  Logger UI:
//
//    TOBDLoggerControl   Start / stop logging buttons +
//                        storage status (file + free space).
//    TOBDLoggerExplorer  TListView of saved .obdlog files
//                        with metadata columns.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 Ernst Reidinga (ERDesigns) and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//------------------------------------------------------------------------------

unit OBD.UI.Logger;

interface

uses
  Winapi.Windows,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.IOUtils,
  Data.Bind.Components,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ComCtrls,
  OBD.UI.Types,
  OBD.UI.GDIP,
  OBD.UI.Theme,
  OBD.UI.Control;

type
  /// <summary>Fires when the user clicks Start / Stop.
  /// </summary>
  TOBDLoggerCommandEvent = procedure(Sender: TObject;
    AStart: Boolean) of object;

  /// <summary>Logger control panel. Paint-driven Start +
  /// Stop hot-spots; clicks fire <see cref="OnCommand"/>.
  /// Storage line shows the active file + free-space figure.
  /// </summary>
  TOBDLoggerControl = class(TOBDCustomControl)
  strict private
    FRunning:        Boolean;
    FFileName:       string;
    FFreeBytes:      Int64;
    FStartRect:      TRect;
    FStopRect:       TRect;
    FFont:           TFont;
    FOnCommand:      TOBDLoggerCommandEvent;
    procedure SetRunning(AValue: Boolean);
    procedure SetFileName(const AValue: string);
    procedure SetFreeBytes(AValue: Int64);
    procedure SetFontA(AValue: TFont);
    procedure HandleFontChange(Sender: TObject);
    procedure NotifyBindings;
    procedure DrawButton(ACanvas: TCanvas;
      AGraphics: TGPGraphics; const ARect: TRect;
      const ACaption: string; AAccent: TColor; AEnabled: Boolean);
    function  FormatBytes(AVal: Int64): string;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    /// <summary>Logging-active flag. Drives the Start / Stop
    /// enabled state. Default False.</summary>
    property Running: Boolean
      read FRunning write SetRunning default False;
    /// <summary>Active log file path (display only).</summary>
    property FileName: string
      read FFileName write SetFileName;
    /// <summary>Free space on the storage volume in bytes;
    /// rendered with auto KB/MB/GB suffix. Default 0.</summary>
    property FreeBytes: Int64
      read FFreeBytes write SetFreeBytes default 0;
    /// <summary>Font used for the captions and storage line.
    /// </summary>
    property LabelFont: TFont read FFont write SetFontA;
    /// <summary>Fires when the user clicks Start
    /// (<c>AStart = True</c>) or Stop (<c>AStart = False</c>).
    /// </summary>
    property OnCommand: TOBDLoggerCommandEvent
      read FOnCommand write FOnCommand;
  end;

  /// <summary>One log-file entry shown by
  /// <see cref="TOBDLoggerExplorer"/>.</summary>
  TOBDLoggerFileInfo = record
    FileName: string;
    Modified: TDateTime;
    SizeBytes: Int64;
    Note:     string;
  end;

  /// <summary>TListView of saved .obdlog files. Host scans
  /// the storage dir + calls <see cref="LoadFiles"/>; the
  /// list is read-only with double-click as the open
  /// trigger via the inherited TListView events.</summary>
  TOBDLoggerExplorer = class(TListView)
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Replace the displayed rows with the supplied
    /// file descriptors.</summary>
    procedure   LoadFiles(
      const AFiles: TArray<TOBDLoggerFileInfo>);
    /// <summary>Remove all rows.</summary>
    procedure   ClearFiles;
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

{ ---- TOBDLoggerControl ----------------------------------------- }

constructor TOBDLoggerControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 360;
  Height := 90;
  FFont := TFont.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 10;
  FFont.OnChange := HandleFontChange;
end;

destructor TOBDLoggerControl.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TOBDLoggerControl.NotifyBindings;
begin
  if ([csDesigning, csDestroying] * ComponentState) <> [] then Exit;
  try
    TBindings.Notify(Self, '');
  except
  end;
end;

procedure TOBDLoggerControl.HandleFontChange(Sender: TObject);
begin
  Repaint;
end;

procedure TOBDLoggerControl.SetRunning(AValue: Boolean);
begin
  if FRunning = AValue then Exit;
  FRunning := AValue; NotifyBindings; Repaint;
end;

procedure TOBDLoggerControl.SetFileName(const AValue: string);
begin
  if FFileName = AValue then Exit;
  FFileName := AValue; NotifyBindings; Repaint;
end;

procedure TOBDLoggerControl.SetFreeBytes(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if FFreeBytes = AValue then Exit;
  FFreeBytes := AValue; NotifyBindings; Repaint;
end;

procedure TOBDLoggerControl.SetFontA(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

function TOBDLoggerControl.FormatBytes(AVal: Int64): string;
begin
  if AVal < 1024 then
    Result := Format('%d B', [AVal])
  else if AVal < 1024 * 1024 then
    Result := Format('%.1f KiB', [AVal / 1024])
  else if AVal < Int64(1024) * 1024 * 1024 then
    Result := Format('%.1f MiB', [AVal / (1024 * 1024)])
  else
    Result := Format('%.1f GiB',
      [AVal / (1024 * 1024 * 1024)]);
end;

procedure TOBDLoggerControl.DrawButton(ACanvas: TCanvas;
  AGraphics: TGPGraphics; const ARect: TRect;
  const ACaption: string; AAccent: TColor; AEnabled: Boolean);
var
  Brush: TGPSolidBrush;
  Pen:   TGPPen;
  R: TGPRectF;
  Col: TColor;
begin
  R.X := ARect.Left;
  R.Y := ARect.Top;
  R.Width  := ARect.Width;
  R.Height := ARect.Height;
  if AEnabled then Col := AAccent
  else             Col := Palette.NeutralLight;
  Brush := TGPSolidBrush.Create(ColorToARGB(Col));
  Pen := TGPPen.Create(ColorToARGB(EffectiveBorder), 1);
  try
    AGraphics.FillRectangle(Brush, R);
    AGraphics.DrawRectangle(Pen, R);
  finally
    Pen.Free;
    Brush.Free;
  end;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := FFont;
  if AEnabled then ACanvas.Font.Color := clWhite
  else             ACanvas.Font.Color := EffectiveForeground;
  ACanvas.TextOut(
    ARect.Left + (ARect.Width  - ACanvas.TextWidth(ACaption)) div 2,
    ARect.Top  + (ARect.Height - ACanvas.TextHeight(ACaption)) div 2,
    ACaption);
end;

procedure TOBDLoggerControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  if PtInRect(FStartRect, Point(X, Y)) and not FRunning then
  begin
    if Assigned(FOnCommand) then
      try
        FOnCommand(Self, True);
      except
      end;
  end
  else if PtInRect(FStopRect, Point(X, Y)) and FRunning then
  begin
    if Assigned(FOnCommand) then
      try
        FOnCommand(Self, False);
      except
      end;
  end;
end;

procedure TOBDLoggerControl.PaintControl(ACanvas: TCanvas);
var
  Graphics: TGPGraphics;
  Pad, ButW, ButH: Integer;
  Storage: string;
begin
  ACanvas.Brush.Style := bsClear;
  Pad := ScaleValue(8);
  ButW := ScaleValue(80);
  ButH := ScaleValue(28);
  FStartRect := Rect(Pad, Pad, Pad + ButW, Pad + ButH);
  FStopRect  := Rect(Pad + ButW + ScaleValue(10), Pad,
    Pad + 2 * ButW + ScaleValue(10), Pad + ButH);

  Graphics := TGPGraphics.Create(ACanvas.Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawButton(ACanvas, Graphics, FStartRect, 'Start',
      Palette.Success, not FRunning);
    DrawButton(ACanvas, Graphics, FStopRect,  'Stop',
      Palette.Danger,  FRunning);
  finally
    Graphics.Free;
  end;

  ACanvas.Font := FFont;
  ACanvas.Font.Color := EffectiveForeground;
  if FFileName <> '' then
    ACanvas.TextOut(Pad, Pad + ButH + ScaleValue(8),
      Format('file: %s', [FFileName]));
  Storage := Format('free: %s', [FormatBytes(FFreeBytes)]);
  ACanvas.TextOut(Width - Pad - ACanvas.TextWidth(Storage),
    Pad + ButH + ScaleValue(8), Storage);
end;

{ ---- TOBDLoggerExplorer --------------------------------------- }

constructor TOBDLoggerExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  RowSelect := True;
  ReadOnly  := True;
  GridLines := True;
  ShowColumnHeaders := True;
end;

procedure TOBDLoggerExplorer.CreateWnd;
begin
  inherited;
  if Columns.Count = 0 then
    ConfigureCols(Self,
      ['File', 'Modified', 'Size', 'Note'],
      [200, 130, 80, 200]);
end;

procedure TOBDLoggerExplorer.LoadFiles(
  const AFiles: TArray<TOBDLoggerFileInfo>);
var
  F: TOBDLoggerFileInfo;
  Item: TListItem;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    for F in AFiles do
    begin
      Item := Items.Add;
      Item.Caption := ExtractFileName(F.FileName);
      Item.SubItems.Add(
        FormatDateTime('yyyy-mm-dd hh:nn', F.Modified));
      if F.SizeBytes < 1024 * 1024 then
        Item.SubItems.Add(Format('%.1f KiB',
          [F.SizeBytes / 1024]))
      else
        Item.SubItems.Add(Format('%.1f MiB',
          [F.SizeBytes / (1024 * 1024)]));
      Item.SubItems.Add(F.Note);
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TOBDLoggerExplorer.ClearFiles;
begin
  Items.Clear;
end;

end.
