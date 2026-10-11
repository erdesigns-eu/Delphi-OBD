//------------------------------------------------------------------------------
//  ERD.UI.RangeEditor
//
//  TOBDRangeEditor - a garage normal-range profile editor for dialogs. It
//  paints the mockup table, uses in-place TOBDEdit children for the selected
//  row, validates Low and High values before writing them to a
//  TOBDRangeProfile and listens for profile changes without taking over the
//  profile's OnChange event.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-10  ERD  Initial implementation for the OBD Studio controls.
//------------------------------------------------------------------------------

unit ERD.UI.RangeEditor;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.Buttons,
  ERD.UI.Edits,
  ERD.UI.ScrollBar,
  ERD.UI.RangeProfiles;

type
  /// <summary>TOBDEdit descendant that can paint a danger validation frame.</summary>
  TOBDRangeEdit = class(TOBDEdit)
  strict private
    FDanger: Boolean;
    procedure SetDanger(AValue: Boolean);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>True when the edit should draw the danger validation frame.</summary>
    property Danger: Boolean read FDanger write SetDanger;
  end;

  /// <summary>Range-profile editor with validation and dialog buttons.</summary>
  TOBDRangeEditor = class(TOBDCustomControl, IOBDSurface)
  strict private
    FRangeProfile: TOBDRangeProfile;
    FPreviewProfile: TOBDRangeProfile;
    FLowEdit: TOBDRangeEdit;
    FHighEdit: TOBDRangeEdit;
    FResetButton: TOBDButton;
    FResetAllButton: TOBDButton;
    FApplyEngines: TOBDCheckBox;
    FSaveButton: TOBDButton;
    FCancelButton: TOBDButton;
    FSelected: Integer;
    FTopRow: Integer;
    FInvalid: Boolean;
    FInvalidMessage: string;
    FUpdatingEdits: Boolean;
    FDraggingScroll: Boolean;
    FDraggingSelect: Boolean;
    FBarHot: Boolean;
    FColumnWidths: array[0..3] of Integer;
    FResizeColumn: Integer;
    FResizeStartX: Integer;
    FResizeStartWidth: Integer;
    FEditHigh: Boolean;
    FScrollDragOffset: Integer;
    FOnSave: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    function GetColumnWidth(AIndex: Integer): Integer;
    procedure SetColumnWidth(AIndex, AValue: Integer);
    function ColumnX(AIndex: Integer): Integer;
    function ColumnDividerAt(X, Y: Integer): Integer;
    procedure SetRangeProfile(AValue: TOBDRangeProfile);
    function ActiveProfile: TOBDRangeProfile;
    procedure EnsurePreviewProfile;
    function RangeCount: Integer;
    function VisibleRows: Integer;
    function RowHeight: Integer;
    function HeaderHeight: Integer;
    function ColumnHeaderHeight: Integer;
    function FooterHeight: Integer;
    function RowTop(AIndex: Integer): Integer;
    function RowAtY(Y: Integer): Integer;
    function LowEditRect(AIndex: Integer): TRect;
    function HighEditRect(AIndex: Integer): TRect;
    function ResetRect(AIndex: Integer): TRect;
    procedure SelectRow(AIndex: Integer);
    procedure LoadSelectedEdits;
    procedure UpdateChildLayout;
    procedure UpdateButtons;
    function EngineApplyCaption: string;
    procedure ScrollTo(ATopRow: Integer);
    function ScrollTrackRect: TRect;
    function ScrollThumbRect: TRect;
    function ParseNumber(const S: string; out AValue: Double): Boolean;
    function ValidateSelected(ACommit: Boolean): Boolean;
    procedure EditChanged(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FocusSelectedEdit(AHigh: Boolean);
    procedure ProfileChanged(Sender: TObject);
    procedure ResetSelectedClick(Sender: TObject);
    procedure ResetAllClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure DrawHeader(APainter: TOBDPainter);
    procedure DrawRows(APainter: TOBDPainter);
    procedure DrawFooter(APainter: TOBDPainter);
    procedure DrawScrollBar(APainter: TOBDPainter);
    procedure SetBarHot(AValue: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    /// <summary>Creates the in-place edits and dialog action buttons.</summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Unregisters the range-profile listener.</summary>
    destructor Destroy; override;
    /// <summary>Repositions edits and buttons for the effective density.</summary>
    procedure DensityChanged; override;
    /// <summary>Reloads rows after the assigned profile changed externally.</summary>
    procedure RangesChanged;
    /// <summary>Surface colour for child edits and buttons.</summary>
    function SurfaceColor: TColor;
    /// <summary>Restores the default column widths.</summary>
    procedure ResetColumnWidths;
  published
    /// <summary>Garage range profile being edited.</summary>
    property RangeProfile: TOBDRangeProfile read FRangeProfile write SetRangeProfile;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>True when density follows the resolved theme.</summary>
    property ParentDensity;
    /// <summary>Parameter column width at 96 DPI; 0 = default. Columns are
    /// resized at run time by dragging the header dividers; double-click a
    /// divider to restore the defaults.</summary>
    property ParameterColumnWidth: Integer index 0 read GetColumnWidth
      write SetColumnWidth default 0;
    /// <summary>Unit column width at 96 DPI; 0 = default.</summary>
    property UnitColumnWidth: Integer index 1 read GetColumnWidth
      write SetColumnWidth default 0;
    /// <summary>Low column width at 96 DPI; 0 = default.</summary>
    property LowColumnWidth: Integer index 2 read GetColumnWidth
      write SetColumnWidth default 0;
    /// <summary>High column width at 96 DPI; 0 = default.</summary>
    property HighColumnWidth: Integer index 3 read GetColumnWidth
      write SetColumnWidth default 0;
    /// <summary>Fires after a valid Save click.</summary>
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    /// <summary>Fires when Cancel is clicked.</summary>
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

implementation

const
  /// <summary>Default and minimum widths (96 DPI) of the Parameter, Unit,
  /// Low and High columns; Source takes the rest of the row.</summary>
  DefaultColumnWidths: array[0..3] of Integer = (214, 60, 100, 100);
  MinColumnWidths: array[0..3] of Integer = (60, 32, 56, 56);
  /// <summary>Space between an in-place edit and the next column.</summary>
  EditColumnGap = 16;

function FormatRangeNumber(AValue: Double; ADecimals: Integer): string;
var
  Mask: string;
begin
  if ADecimals <= 0 then
    Mask := '0'
  else
    Mask := '0.' + StringOfChar('#', Max(1, ADecimals));
  Result := FormatFloat(Mask, AValue);
end;

{ TOBDRangeEdit -------------------------------------------------------------- }

procedure TOBDRangeEdit.SetDanger(AValue: Boolean);
begin
  if FDanger = AValue then
    Exit;
  FDanger := AValue;
  Invalidate;
end;

procedure TOBDRangeEdit.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  inherited PaintControl(ACanvas);
  if not FDanger then
    Exit;
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FrameRect(Rect(0, 0, Width, Height), Palette.Danger, ScaleValue(2));
  finally
    P.Free;
  end;
end;

{ TOBDRangeEditor ------------------------------------------------------------ }

constructor TOBDRangeEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPreviewProfile := TOBDRangeProfile.Create(Self);
  FSelected := -1;
  FTopRow := 0;
  FResizeColumn := -1;
  TabStop := True;
  Width := 840;
  Height := 520;

  FLowEdit := TOBDRangeEdit.Create(Self);
  FLowEdit.Parent := Self;
  FLowEdit.Alignment := taRightJustify;
  FLowEdit.AutoSize := False;
  FLowEdit.OnChange := EditChanged;
  FLowEdit.OnEnter := EditEnter;
  FLowEdit.OnExit := EditExit;
  FLowEdit.OnKeyDown := EditKeyDown;

  FHighEdit := TOBDRangeEdit.Create(Self);
  FHighEdit.Parent := Self;
  FHighEdit.Alignment := taRightJustify;
  FHighEdit.AutoSize := False;
  FHighEdit.OnChange := EditChanged;
  FHighEdit.OnEnter := EditEnter;
  FHighEdit.OnExit := EditExit;
  FHighEdit.OnKeyDown := EditKeyDown;

  FResetButton := TOBDButton.Create(Self);
  FResetButton.Parent := Self;
  FResetButton.Kind := bkGhost;
  FResetButton.Caption := 'Reset';
  FResetButton.AutoSize := False;
  FResetButton.OnClick := ResetSelectedClick;

  FResetAllButton := TOBDButton.Create(Self);
  FResetAllButton.Parent := Self;
  FResetAllButton.Kind := bkGhost;
  FResetAllButton.Caption := 'Reset all';
  FResetAllButton.AutoSize := False;
  FResetAllButton.OnClick := ResetAllClick;

  FApplyEngines := TOBDCheckBox.Create(Self);
  FApplyEngines.Parent := Self;
  FApplyEngines.Caption := 'Apply to engine codes';
  FApplyEngines.Checked := True;
  FApplyEngines.AutoSize := True;

  FSaveButton := TOBDButton.Create(Self);
  FSaveButton.Parent := Self;
  FSaveButton.Kind := bkPrimary;
  FSaveButton.Caption := 'Save profile';
  FSaveButton.AutoSize := False;
  FSaveButton.OnClick := SaveClick;

  FCancelButton := TOBDButton.Create(Self);
  FCancelButton.Parent := Self;
  FCancelButton.Kind := bkSecondary;
  FCancelButton.Caption := 'Cancel';
  FCancelButton.AutoSize := False;
  FCancelButton.OnClick := CancelClick;

  UpdateChildLayout;
end;

destructor TOBDRangeEditor.Destroy;
begin
  if FRangeProfile <> nil then
  begin
    FRangeProfile.RemoveChangeListener(ProfileChanged);
    FRangeProfile.RemoveFreeNotification(Self);
  end;
  inherited Destroy;
end;

procedure TOBDRangeEditor.DensityChanged;
begin
  inherited DensityChanged;
  UpdateChildLayout;
end;

procedure TOBDRangeEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRangeProfile) then
    FRangeProfile := nil;
end;

procedure TOBDRangeEditor.SetRangeProfile(AValue: TOBDRangeProfile);
begin
  if FRangeProfile = AValue then
    Exit;
  if FRangeProfile <> nil then
  begin
    FRangeProfile.RemoveChangeListener(ProfileChanged);
    FRangeProfile.RemoveFreeNotification(Self);
  end;
  FRangeProfile := AValue;
  if FRangeProfile <> nil then
  begin
    FRangeProfile.FreeNotification(Self);
    FRangeProfile.AddChangeListener(ProfileChanged);
  end;
  FSelected := IfThen(RangeCount > 0, 0, -1);
  FTopRow := 0;
  FInvalid := False;
  LoadSelectedEdits;
  UpdateChildLayout;
  Invalidate;
end;

function TOBDRangeEditor.ActiveProfile: TOBDRangeProfile;
begin
  Result := FRangeProfile;
  if (Result = nil) and IsPreview then
  begin
    EnsurePreviewProfile;
    Result := FPreviewProfile;
  end;
end;

procedure TOBDRangeEditor.EnsurePreviewProfile;
var
  R: TOBDValueRange;
begin
  if FPreviewProfile.Ranges.Count > 0 then
    Exit;
  FPreviewProfile.LoadDefaults;
  FPreviewProfile.ProfileName := 'VAG 1.6 TDI CR (garage)';
  FPreviewProfile.Vehicle := 'VW Golf / Caddy 1.6 TDI';
  FPreviewProfile.EngineCodes := 'CLHA, CRKB and DDYA';
  R := FPreviewProfile.Ranges.FindKey('egr_error');
  if R <> nil then
  begin
    R.Low := -15;
    R.High := 15;
  end;
  R := FPreviewProfile.Ranges.FindKey('dpf_dp');
  if R <> nil then
    R.High := 15;
  R := FPreviewProfile.Ranges.FindKey('calculated_load');
  if R <> nil then
    R.High := 82;
end;

function TOBDRangeEditor.RangeCount: Integer;
var
  P: TOBDRangeProfile;
begin
  P := ActiveProfile;
  if P <> nil then
    Result := P.Ranges.Count
  else
    Result := 0;
end;

function TOBDRangeEditor.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.Row);
end;

function TOBDRangeEditor.HeaderHeight: Integer;
begin
  Result := ScaleValue(96);
end;

function TOBDRangeEditor.ColumnHeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.ColHead);
end;

function TOBDRangeEditor.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot + 16);
end;

function TOBDRangeEditor.VisibleRows: Integer;
var
  Space: Integer;
begin
  Space := Height - HeaderHeight - ColumnHeaderHeight - FooterHeight;
  Result := Max(1, Space div RowHeight);
end;

function TOBDRangeEditor.RowTop(AIndex: Integer): Integer;
begin
  Result := HeaderHeight + ColumnHeaderHeight + (AIndex - FTopRow) * RowHeight;
end;

function TOBDRangeEditor.RowAtY(Y: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := FTopRow + (Y - HeaderHeight - ColumnHeaderHeight) div RowHeight;
  if (I >= FTopRow) and (I < RangeCount) and (Y >= RowTop(I)) and
    (Y < RowTop(I) + RowHeight) then
    Result := I;
end;

function TOBDRangeEditor.GetColumnWidth(AIndex: Integer): Integer;
begin
  Result := FColumnWidths[AIndex];
end;

procedure TOBDRangeEditor.SetColumnWidth(AIndex, AValue: Integer);
begin
  if AValue > 0 then
    AValue := Max(MinColumnWidths[AIndex], AValue)
  else
    AValue := 0;
  if FColumnWidths[AIndex] = AValue then
    Exit;
  FColumnWidths[AIndex] := AValue;
  // The in-place edits follow their columns.
  UpdateChildLayout;
  Invalidate;
end;

procedure TOBDRangeEditor.ResetColumnWidths;
var
  I: Integer;
begin
  for I := Low(FColumnWidths) to High(FColumnWidths) do
    FColumnWidths[I] := 0;
  UpdateChildLayout;
  Invalidate;
end;

function TOBDRangeEditor.ColumnX(AIndex: Integer): Integer;
var
  I, W: Integer;
begin
  // Left edge of column AIndex: 0 Parameter, 1 Unit, 2 Low, 3 High,
  // 4 Source.
  Result := ScaleValue(16);
  for I := 0 to Min(AIndex, 4) - 1 do
  begin
    W := FColumnWidths[I];
    if W <= 0 then
      W := DefaultColumnWidths[I];
    Inc(Result, ScaleValue(W));
  end;
end;

function TOBDRangeEditor.ColumnDividerAt(X, Y: Integer): Integer;
var
  I, Slop: Integer;
begin
  Result := -1;
  if (Y < HeaderHeight) or (Y >= HeaderHeight + ColumnHeaderHeight) then
    Exit;
  Slop := ScaleValue(4);
  // Dividers sit half-way into the gap before each column. Right-most
  // first, so a narrowed column can still be widened.
  for I := High(FColumnWidths) downto Low(FColumnWidths) do
    if Abs(X - (ColumnX(I + 1) - ScaleValue(8))) <= Slop then
      Exit(I);
end;

function TOBDRangeEditor.LowEditRect(AIndex: Integer): TRect;
var
  Top, EH: Integer;
begin
  EH := ScaleValue(Metrics.Edit);
  Top := RowTop(AIndex) + (RowHeight - EH) div 2;
  Result := Rect(ColumnX(2), Top, ColumnX(3) - ScaleValue(EditColumnGap),
    Top + EH);
end;

function TOBDRangeEditor.HighEditRect(AIndex: Integer): TRect;
var
  Top, EH: Integer;
begin
  EH := ScaleValue(Metrics.Edit);
  Top := RowTop(AIndex) + (RowHeight - EH) div 2;
  Result := Rect(ColumnX(3), Top, ColumnX(4) - ScaleValue(EditColumnGap),
    Top + EH);
end;

function TOBDRangeEditor.ResetRect(AIndex: Integer): TRect;
var
  W, H, Top: Integer;
begin
  W := ScaleValue(78);
  H := ScaleValue(Metrics.Button);
  Top := RowTop(AIndex) + (RowHeight - H) div 2;
  Result := Rect(Width - ScaleValue(16) - W, Top, Width - ScaleValue(16), Top + H);
end;

procedure TOBDRangeEditor.SelectRow(AIndex: Integer);
begin
  if AIndex = FSelected then
    Exit;
  if not ValidateSelected(True) then
    Exit;
  FSelected := EnsureRange(AIndex, -1, RangeCount - 1);
  if FSelected >= 0 then
  begin
    if FSelected < FTopRow then
      FTopRow := FSelected
    else if FSelected >= FTopRow + VisibleRows then
      FTopRow := FSelected - VisibleRows + 1;
  end;
  FInvalid := False;
  LoadSelectedEdits;
  UpdateChildLayout;
  Invalidate;
end;

procedure TOBDRangeEditor.LoadSelectedEdits;
var
  P: TOBDRangeProfile;
  R: TOBDValueRange;
begin
  P := ActiveProfile;
  FUpdatingEdits := True;
  try
    if P <> nil then
    begin
      if (FSelected >= 0) and (FSelected < P.Ranges.Count) then
      begin
        R := P.Ranges[FSelected];
        FLowEdit.Text := FormatRangeNumber(R.Low, R.Decimals);
        FHighEdit.Text := FormatRangeNumber(R.High, R.Decimals);
        Exit;
      end;
    end;
    FLowEdit.Text := '';
    FHighEdit.Text := '';
  finally
    FUpdatingEdits := False;
  end;
end;

procedure TOBDRangeEditor.UpdateChildLayout;
var
  VisibleEditor: Boolean;
  R: TRect;
  FH, BH, WSave, WCancel, WResetAll, Y, MidY, Ring, SaveX, CancelX: Integer;
  P: TOBDRangeProfile;
begin
  P := ActiveProfile;
  if (FSelected < 0) and (RangeCount > 0) then
    FSelected := 0;
  if FTopRow > Max(0, RangeCount - VisibleRows) then
    FTopRow := Max(0, RangeCount - VisibleRows);

  VisibleEditor := False;
  if P <> nil then
    VisibleEditor := (FSelected >= FTopRow) and
      (FSelected < FTopRow + VisibleRows) and (FSelected < P.Ranges.Count);
  FLowEdit.Visible := VisibleEditor;
  FHighEdit.Visible := VisibleEditor;
  FResetButton.Visible := False;
  if VisibleEditor then
    FResetButton.Visible := P.Ranges[FSelected].IsModified;
  if VisibleEditor then
  begin
    R := LowEditRect(FSelected);
    FLowEdit.SetBounds(R.Left, R.Top, R.Width, R.Height);
    R := HighEditRect(FSelected);
    FHighEdit.SetBounds(R.Left, R.Top, R.Width, R.Height);
    R := ResetRect(FSelected);
    Ring := ScaleValue(4);
    FResetButton.SetBounds(R.Left - Ring, R.Top - Ring, R.Width + 2 * Ring,
      R.Height + 2 * Ring);
  end;

  FH := FooterHeight;
  BH := ScaleValue(Metrics.Button);
  Y := Height - FH + (FH - BH) div 2;
  MidY := Height - FH + FH div 2;
  Ring := ScaleValue(4);
  WResetAll := ScaleValue(104);
  WSave := ScaleValue(118);
  WCancel := ScaleValue(92);
  SaveX := Width - ScaleValue(16) - WSave;
  CancelX := SaveX - ScaleValue(8) - WCancel;
  FSaveButton.SetBounds(SaveX - Ring, Y - Ring, WSave + 2 * Ring,
    BH + 2 * Ring);
  FCancelButton.SetBounds(CancelX - Ring, Y - Ring, WCancel + 2 * Ring,
    BH + 2 * Ring);
  FApplyEngines.Caption := EngineApplyCaption;
  FApplyEngines.AdjustSize;
  FApplyEngines.SetBounds(ScaleValue(16) - Ring,
    MidY - FApplyEngines.Height div 2, Min(FApplyEngines.Width,
    Max(0, CancelX - ScaleValue(16) - ScaleValue(160))),
    FApplyEngines.Height);
  FResetAllButton.SetBounds(FApplyEngines.BoundsRect.Right + ScaleValue(14),
    Y - Ring,
    WResetAll + 2 * Ring, BH + 2 * Ring);
  UpdateButtons;
end;

procedure TOBDRangeEditor.UpdateButtons;
begin
  FLowEdit.Danger := FInvalid;
  FHighEdit.Danger := FInvalid;
  FSaveButton.Enabled := not FInvalid;
  FResetAllButton.Enabled := ActiveProfile <> nil;
  FSaveButton.Enabled := FSaveButton.Enabled and (ActiveProfile <> nil);
end;

function TOBDRangeEditor.EngineApplyCaption: string;
var
  P: TOBDRangeProfile;
begin
  P := ActiveProfile;
  Result := 'Apply to all matching engines';
  if P = nil then
    Exit;
  if Trim(P.EngineCodes) <> '' then
    Result := 'Apply to ' + P.EngineCodes + ' engines';
end;

procedure TOBDRangeEditor.ScrollTo(ATopRow: Integer);
begin
  ATopRow := EnsureRange(ATopRow, 0, Max(0, RangeCount - VisibleRows));
  if FTopRow = ATopRow then
    Exit;
  FTopRow := ATopRow;
  UpdateChildLayout;
  Invalidate;
end;

function TOBDRangeEditor.ScrollTrackRect: TRect;
var
  W, TrackTop: Integer;
begin
  if RangeCount <= VisibleRows then
    Exit(TRect.Empty);
  W := ScaleValue(10);
  TrackTop := HeaderHeight + ColumnHeaderHeight;
  Result := Rect(Width - W - ScaleValue(2), TrackTop, Width - ScaleValue(2),
    Height - FooterHeight);
end;

function TOBDRangeEditor.ScrollThumbRect: TRect;
var
  Track: TRect;
begin
  Track := ScrollTrackRect;
  if Track.IsEmpty then
    Exit(TRect.Empty);
  Result := OBDScrollThumbRect(Track, True, 0, RangeCount - 1, VisibleRows,
    FTopRow, FBarHot, FDraggingScroll);
end;

function TOBDRangeEditor.ParseNumber(const S: string; out AValue: Double): Boolean;
var
  T: string;
begin
  T := Trim(S);
  Result := TryStrToFloat(T, AValue);
  if not Result then
  begin
    T := StringReplace(T, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
    T := StringReplace(T, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
    Result := TryStrToFloat(T, AValue);
  end;
end;

function TOBDRangeEditor.ValidateSelected(ACommit: Boolean): Boolean;
var
  P: TOBDRangeProfile;
  R: TOBDValueRange;
  Low, High: Double;
begin
  Result := True;
  if FUpdatingEdits then
    Exit;
  P := ActiveProfile;
  if P = nil then
    Exit;
  if (FSelected < 0) or (FSelected >= P.Ranges.Count) then
    Exit;
  R := P.Ranges[FSelected];
  FInvalid := False;
  FInvalidMessage := '';
  if not ParseNumber(FLowEdit.Text, Low) then
  begin
    FInvalid := True;
    FInvalidMessage := 'Low is not a valid number.';
  end
  else if not ParseNumber(FHighEdit.Text, High) then
  begin
    FInvalid := True;
    FInvalidMessage := 'High is not a valid number.';
  end
  else if Low > High then
  begin
    FInvalid := True;
    FInvalidMessage := 'Low must be less than or equal to High.';
  end
  else if (Low < R.Min) or (Low > R.Max) or (High < R.Min) or (High > R.Max) then
  begin
    FInvalid := True;
    FInvalidMessage := Format('Values must be within %s..%s.',
      [FormatRangeNumber(R.Min, R.Decimals), FormatRangeNumber(R.Max, R.Decimals)]);
  end;
  Result := not FInvalid;
  if Result and ACommit then
  begin
    R.Low := Low;
    R.High := High;
    LoadSelectedEdits;
  end;
  UpdateButtons;
  Invalidate;
end;

procedure TOBDRangeEditor.EditChanged(Sender: TObject);
begin
  if not FUpdatingEdits then
    ValidateSelected(False);
end;

procedure TOBDRangeEditor.EditEnter(Sender: TObject);
begin
  // Remember the column being edited, so selecting another row by mouse
  // or keyboard keeps editing the same one.
  FEditHigh := Sender = FHighEdit;
end;

procedure TOBDRangeEditor.EditExit(Sender: TObject);
begin
  ValidateSelected(True);
end;

procedure TOBDRangeEditor.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Target: Integer;
begin
  if (ssAlt in Shift) or (ssCtrl in Shift) then
    Exit;
  case Key of
    VK_UP:
      Target := Max(0, FSelected - 1);
    VK_DOWN:
      Target := Min(RangeCount - 1, FSelected + 1);
  else
    Exit;
  end;
  Key := 0;
  // Up/Down step to the neighbouring range and keep editing the same column,
  // so a list of ranges can be typed in one after another.
  SelectRow(Target);
  FocusSelectedEdit(Sender = FHighEdit);
end;

procedure TOBDRangeEditor.FocusSelectedEdit(AHigh: Boolean);
var
  Edit: TOBDRangeEdit;
begin
  if AHigh then
    Edit := FHighEdit
  else
    Edit := FLowEdit;
  if not Edit.Visible or not Edit.CanFocus then
    Exit;
  Edit.SetFocus;
  Edit.SelectAll;
end;

procedure TOBDRangeEditor.ProfileChanged(Sender: TObject);
begin
  RangesChanged;
end;

procedure TOBDRangeEditor.ResetSelectedClick(Sender: TObject);
var
  P: TOBDRangeProfile;
begin
  P := ActiveProfile;
  if P = nil then
    Exit;
  if (FSelected < 0) or (FSelected >= P.Ranges.Count) then
    Exit;
  P.Ranges[FSelected].ResetToDefault;
  FInvalid := False;
  LoadSelectedEdits;
  UpdateChildLayout;
  Invalidate;
end;

procedure TOBDRangeEditor.ResetAllClick(Sender: TObject);
var
  P: TOBDRangeProfile;
begin
  P := ActiveProfile;
  if P = nil then
    Exit;
  P.ResetAll;
  FInvalid := False;
  LoadSelectedEdits;
  UpdateChildLayout;
  Invalidate;
end;

procedure TOBDRangeEditor.SaveClick(Sender: TObject);
begin
  if ValidateSelected(True) and Assigned(FOnSave) then
    FOnSave(Self);
end;

procedure TOBDRangeEditor.CancelClick(Sender: TObject);
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TOBDRangeEditor.RangesChanged;
begin
  if FSelected >= RangeCount then
    FSelected := RangeCount - 1;
  if FSelected < 0 then
    FSelected := IfThen(RangeCount > 0, 0, -1);
  FInvalid := False;
  LoadSelectedEdits;
  UpdateChildLayout;
  Invalidate;
end;

function TOBDRangeEditor.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDRangeEditor.DrawHeader(APainter: TOBDPainter);
var
  P: TOBDRangeProfile;
  R, Box: TRect;
  Text: string;
begin
  P := ActiveProfile;
  APainter.Text(ScaleValue(16), ScaleValue(28), 'Normal ranges', 16,
    Palette.ForegroundText, twBold);
  APainter.Text(ScaleValue(16), ScaleValue(52),
    'Used for the range bars in the freeze frame and the live-data limits.',
    12, Palette.GaugeLabel);
  APainter.Text(ScaleValue(16), ScaleValue(70),
    'Garage values override the built-in defaults for this profile.', 12,
    Palette.GaugeLabel);

  R := Rect(Width - ScaleValue(16 + 260), ScaleValue(34), Width - ScaleValue(16),
    ScaleValue(34) + ScaleValue(Metrics.Edit));
  APainter.Caps(R.Left, ScaleValue(22), 'Profile');
  APainter.EditFrame(R, False, True);
  if P <> nil then
    Text := P.ProfileName
  else
    Text := '';
  if Text = '' then
    Text := '(no profile)';
  APainter.Text(R.Left + ScaleValue(10), R.Top + R.Height div 2, Text, 12.5,
    Palette.ForegroundText, twRegular, taLeftJustify, R.Width - ScaleValue(36));
  Box := R;
  APainter.GlyphChevron(Box.Right - ScaleValue(14), Box.Top + Box.Height / 2,
    Palette.Subtle, True);
end;

procedure TOBDRangeEditor.DrawRows(APainter: TOBDPainter);
var
  P: TOBDRangeProfile;
  I, Last, Top, Mid, EH: Integer;
  R: TOBDValueRange;
  Garage: Boolean;
  RowR, LowR, HighR, RR: TRect;
  ColParam, ColUnit, ColLow, ColHigh, ColSrc: Integer;
  CWidth, HY: Integer;
  Weight: TOBDTextWeight;
  Strength: Single;
begin
  P := ActiveProfile;
  APainter.FillRect(Rect(ScaleValue(1), HeaderHeight, Width - ScaleValue(1),
    HeaderHeight + ColumnHeaderHeight), APainter.HeaderFill);
  APainter.HLine(ScaleValue(1), HeaderHeight + ColumnHeaderHeight - ScaleValue(1),
    Width - ScaleValue(2), Palette.NeutralLight);
  ColParam := ColumnX(0);
  ColUnit := ColumnX(1);
  ColLow := ColumnX(2);
  ColHigh := ColumnX(3);
  ColSrc := ColumnX(4);
  HY := HeaderHeight + ColumnHeaderHeight div 2;
  APainter.Caps(ColParam, HY, 'Parameter', taLeftJustify, clDefault,
    Max(1, ColUnit - ColParam - ScaleValue(12)));
  APainter.Caps(ColUnit, HY, 'Unit', taLeftJustify, clDefault,
    Max(1, ColLow - ColUnit - ScaleValue(12)));
  APainter.Caps(ColLow, HY, 'Low', taLeftJustify, clDefault,
    Max(1, ColHigh - ColLow - ScaleValue(12)));
  APainter.Caps(ColHigh, HY, 'High', taLeftJustify, clDefault,
    Max(1, ColSrc - ColHigh - ScaleValue(12)));
  APainter.Caps(ColSrc, HY, 'Source', taLeftJustify, clDefault,
    Max(1, Width - ColSrc - ScaleValue(16)));
  // Column dividers double as resize grips.
  for I := 1 to 4 do
    APainter.VLine(ColumnX(I) - ScaleValue(8), HeaderHeight + ScaleValue(6),
      ColumnHeaderHeight - ScaleValue(12), Palette.NeutralLight);

  if P = nil then
    Exit;
  Last := Min(P.Ranges.Count - 1, FTopRow + VisibleRows - 1);
  EH := ScaleValue(Metrics.Edit);
  for I := FTopRow to Last do
  begin
    R := P.Ranges[I];
    Top := RowTop(I);
    Mid := Top + RowHeight div 2;
    RowR := Rect(ScaleValue(1), Top, Width - ScaleValue(1), Top + RowHeight);
    Garage := R.IsModified;
    if I = FSelected then
    begin
      if APainter.Dark then
        Strength := 0.12
      else
        Strength := 0.08;
      APainter.FillRect(RowR, APainter.Tint(Palette.Accent, Strength));
    end;
    if Garage then
      APainter.FillRect(Rect(ScaleValue(1), Top + ScaleValue(6), ScaleValue(5),
        Top + RowHeight - ScaleValue(6)), APainter.AccentText);
    if FInvalid and (I = FSelected) then
    begin
      APainter.FillRect(Rect(ScaleValue(1), Top, ScaleValue(4), Top + RowHeight),
        Palette.Danger);
      APainter.Text(ColSrc, Mid, FInvalidMessage, 11.5, Palette.Danger,
        twSemibold, taLeftJustify, Max(0, Width - ColSrc - ScaleValue(110)));
    end;

    APainter.Text(ColParam, Mid, R.Caption, 13, Palette.ForegroundText,
      twRegular, taLeftJustify, ColUnit - ColParam - ScaleValue(14));
    APainter.Text(ColUnit, Mid, R.UnitText, 12.5, Palette.GaugeLabel,
      twRegular, taLeftJustify, Max(1, ColLow - ColUnit - ScaleValue(12)));
    LowR := LowEditRect(I);
    HighR := HighEditRect(I);
    if I <> FSelected then
    begin
      APainter.EditFrame(LowR, False, True);
      if Garage and (not SameValue(R.Low, R.DefaultLow)) then
        Weight := twSemibold
      else
        Weight := twRegular;
      APainter.Text(LowR.Right - ScaleValue(10), LowR.Top + EH div 2,
        FormatRangeNumber(R.Low, R.Decimals), 12.5, Palette.ForegroundText,
        Weight, taRightJustify);
      APainter.EditFrame(HighR, False, True);
      if Garage and (not SameValue(R.High, R.DefaultHigh)) then
        Weight := twSemibold
      else
        Weight := twRegular;
      APainter.Text(HighR.Right - ScaleValue(10), HighR.Top + EH div 2,
        FormatRangeNumber(R.High, R.Decimals), 12.5, Palette.ForegroundText,
        Weight, taRightJustify);
    end;

    if not (FInvalid and (I = FSelected)) then
    begin
      if Garage then
      begin
        CWidth := APainter.Chip(ColSrc, Mid - ScaleValue(10), 'GARAGE',
          APainter.AccentText);
        APainter.Text(ColSrc + CWidth + ScaleValue(8), Mid,
          Format('default %s – %s', [FormatRangeNumber(R.DefaultLow, R.Decimals),
          FormatRangeNumber(R.DefaultHigh, R.Decimals)]), 11.5, Palette.GaugeLabel);
        if I <> FSelected then
        begin
          RR := ResetRect(I);
          APainter.Button(RR, 'Reset', bkGhost, cstNormal, False, glNone);
        end;
      end
      else
        APainter.Chip(ColSrc, Mid - ScaleValue(10), 'DEFAULT', Palette.Subtle);
    end;
    APainter.HLine(ScaleValue(1), Top + RowHeight - ScaleValue(1),
      Width - ScaleValue(2), Palette.NeutralLight);
  end;
end;

procedure TOBDRangeEditor.DrawFooter(APainter: TOBDPainter);
var
  P: TOBDRangeProfile;
  FY, MY: Integer;
  Text: string;
  MsgColor: TColor;
begin
  P := ActiveProfile;
  FY := Height - FooterHeight;
  MY := FY + FooterHeight div 2;
  APainter.HLine(ScaleValue(1), FY, Width - ScaleValue(2), Palette.NeutralLight);
  if P <> nil then
  begin
    Text := Format('%d values differ from the defaults', [P.ModifiedCount]);
    if FInvalid then
      Text := 'Fix invalid rows before saving';
    if FInvalid then
      MsgColor := Palette.Danger
    else
      MsgColor := Palette.GaugeLabel;
    APainter.Text(FCancelButton.Left + ScaleValue(4) - ScaleValue(16), MY,
      Text, 12,
      MsgColor, twRegular, taRightJustify);
  end;
end;

procedure TOBDRangeEditor.DrawScrollBar(APainter: TOBDPainter);
var
  Track: TRect;
begin
  Track := ScrollTrackRect;
  if Track.IsEmpty then
    Exit;
  OBDPaintScrollBar(APainter, Track, True, 0, RangeCount - 1, VisibleRows,
    FTopRow, FBarHot, FDraggingScroll);
end;

procedure TOBDRangeEditor.SetBarHot(AValue: Boolean);
begin
  if FBarHot = AValue then
    Exit;
  FBarHot := AValue;
  Invalidate;
end;

procedure TOBDRangeEditor.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not FDraggingScroll then
    SetBarHot(False);
end;

procedure TOBDRangeEditor.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(ClientRect);
    DrawHeader(P);
    DrawRows(P);
    DrawFooter(P);
    DrawScrollBar(P);
  finally
    P.Free;
  end;
end;

procedure TOBDRangeEditor.Resize;
begin
  inherited Resize;
  UpdateChildLayout;
end;

procedure TOBDRangeEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
  P: TOBDRangeProfile;
  Track, Thumb: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  FDraggingSelect := False;
  if Button <> mbLeft then
    Exit;
  SetFocus;
  if ColumnDividerAt(X, Y) >= 0 then
  begin
    // The second press of a double-click is left to DblClick (reset).
    if ssDouble in Shift then
      Exit;
    FResizeColumn := ColumnDividerAt(X, Y);
    FResizeStartX := X;
    FResizeStartWidth := MulDiv(ColumnX(FResizeColumn + 1) -
      ColumnX(FResizeColumn), 96, ScaleValue(96));
    MouseCapture := True;
    Exit;
  end;
  Track := ScrollTrackRect;
  if not Track.IsEmpty and (X >= Track.Left - ScaleValue(4)) and
    (Y >= Track.Top) and (Y < Track.Bottom) then
  begin
    if not ValidateSelected(True) then
      Exit;
    Thumb := ScrollThumbRect;
    if Y < Thumb.Top then
      ScrollTo(FTopRow - VisibleRows)
    else if Y >= Thumb.Bottom then
      ScrollTo(FTopRow + VisibleRows)
    else
    begin
      FDraggingScroll := True;
      FScrollDragOffset := Y - Thumb.Top;
      Invalidate;
    end;
    Exit;
  end;
  I := RowAtY(Y);
  if I < 0 then
    Exit;
  P := ActiveProfile;
  if P <> nil then
  begin
    if PtInRect(ResetRect(I), Point(X, Y)) and P.Ranges[I].IsModified then
    begin
      SelectRow(I);
      if not FInvalid then
        ResetSelectedClick(Self);
      Exit;
    end;
  end;
  // A click on a row's low or high value picks that column; elsewhere on
  // the row the last edited column is kept.
  if PtInRect(HighEditRect(I), Point(X, Y)) then
    FEditHigh := True
  else if PtInRect(LowEditRect(I), Point(X, Y)) then
    FEditHigh := False;
  SelectRow(I);
  if FSelected = I then
  begin
    FocusSelectedEdit(FEditHigh);
    // Holding the button and sweeping over rows moves the selection, as
    // in the inspector.
    FDraggingSelect := True;
  end;
end;

procedure TOBDRangeEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Track, Thumb: TRect;
  RowsTop, RowsBottom, I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if FResizeColumn >= 0 then
  begin
    SetColumnWidth(FResizeColumn, Max(MinColumnWidths[FResizeColumn],
      FResizeStartWidth + MulDiv(X - FResizeStartX, 96, ScaleValue(96))));
    Exit;
  end;
  if ColumnDividerAt(X, Y) >= 0 then
    Cursor := crHSplit
  else
    Cursor := crDefault;
  if FDraggingSelect then
  begin
    if not (ssLeft in Shift) or (RangeCount = 0) then
    begin
      FDraggingSelect := False;
      Exit;
    end;
    // Above or below the rows the selection steps on, which scrolls.
    RowsTop := HeaderHeight + ColumnHeaderHeight;
    RowsBottom := RowsTop + Min(VisibleRows, RangeCount - FTopRow) *
      RowHeight;
    if Y < RowsTop then
      I := FTopRow - 1
    else if Y >= RowsBottom then
      I := FTopRow + VisibleRows
    else
      I := RowAtY(Y);
    I := EnsureRange(I, 0, RangeCount - 1);
    if I <> FSelected then
    begin
      SelectRow(I);
      // Stop when the current row is invalid and keeps the selection.
      if FSelected <> I then
        FDraggingSelect := False
      else
        FocusSelectedEdit(FEditHigh);
    end;
    Exit;
  end;
  if not FDraggingScroll then
  begin
    Track := ScrollTrackRect;
    SetBarHot(not Track.IsEmpty and (X >= Track.Left - ScaleValue(4)) and
      (Y >= Track.Top) and (Y < Track.Bottom));
    Exit;
  end;
  Track := ScrollTrackRect;
  Thumb := ScrollThumbRect;
  if Track.Height - Thumb.Height > 0 then
    ScrollTo(Round((Y - FScrollDragOffset - Track.Top) *
      Max(0, RangeCount - VisibleRows) / (Track.Height - Thumb.Height)));
end;

procedure TOBDRangeEditor.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FDraggingSelect := False;
  if FResizeColumn >= 0 then
  begin
    FResizeColumn := -1;
    MouseCapture := False;
  end;
  if FDraggingScroll then
  begin
    FDraggingScroll := False;
    SetBarHot(PtInRect(ScrollTrackRect, Point(X, Y)));
    Invalidate;
  end;
end;

procedure TOBDRangeEditor.DblClick;
var
  P: TPoint;
begin
  inherited DblClick;
  P := ScreenToClient(Mouse.CursorPos);
  // Double-clicking a header divider restores the default widths.
  if ColumnDividerAt(P.X, P.Y) >= 0 then
    ResetColumnWidths;
end;

procedure TOBDRangeEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP:
      SelectRow(Max(0, FSelected - 1));
    VK_DOWN:
      SelectRow(Min(RangeCount - 1, FSelected + 1));
    VK_PRIOR:
      SelectRow(Max(0, FSelected - VisibleRows));
    VK_NEXT:
      SelectRow(Min(RangeCount - 1, FSelected + VisibleRows));
    VK_HOME:
      SelectRow(0);
    VK_END:
      SelectRow(RangeCount - 1);
  else
    Exit;
  end;
  Key := 0;
  FocusSelectedEdit(FEditHigh);
end;

procedure TOBDRangeEditor.CMMouseWheel(var Message: TCMMouseWheel);
begin
  if not ValidateSelected(True) then
  begin
    Message.Result := 1;
    Exit;
  end;
  ScrollTo(FTopRow - Sign(Message.WheelDelta) * 3);
  Message.Result := 1;
end;

procedure TOBDRangeEditor.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
