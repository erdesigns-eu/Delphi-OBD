//------------------------------------------------------------------------------
//  ERD.UI.PreconditionList
//
//  TOBDPreconditionList - density-aware OBD Studio Pro card that lists live
//  procedure or write preconditions with pass, fail, pending and skipped states.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.PreconditionList;

interface

uses
  Winapi.Windows,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>State of a precondition row.</summary>
  TOBDPreconditionState = (
    /// <summary>The required condition is satisfied.</summary>
    pcnMet,
    /// <summary>The required condition is not satisfied.</summary>
    pcnFailed,
    /// <summary>The condition is still being checked or is in progress.</summary>
    pcnPending,
    /// <summary>The condition does not apply to the current procedure.</summary>
    pcnNotRequired);

  TOBDPreconditionList = class;

  /// <summary>One condition row shown by <see cref="TOBDPreconditionList"/>.</summary>
  TOBDPreconditionItem = class(TCollectionItem)
  strict private
    FState: TOBDPreconditionState;
    FLabelText: string;
    FValueText: string;
    procedure SetState(AValue: TOBDPreconditionState);
    procedure SetLabelText(const AValue: string);
    procedure SetValueText(const AValue: string);
  protected
    /// <summary>Returns the row label for the collection editor.</summary>
    /// <returns>Display name.</returns>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a pending precondition row.</summary>
    /// <param name="Collection">Owner collection.</param>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies the row state and text.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Current precondition state.</summary>
    property State: TOBDPreconditionState read FState write SetState
      default pcnPending;
    /// <summary>Mechanic-facing condition label.</summary>
    property LabelText: string read FLabelText write SetLabelText;
    /// <summary>Live value text shown on the right.</summary>
    property ValueText: string read FValueText write SetValueText;
  end;

  /// <summary>Streamable collection of precondition rows.</summary>
  TOBDPreconditionCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDPreconditionItem;
    procedure SetItem(Index: Integer; AValue: TOBDPreconditionItem);
  protected
    /// <summary>Notifies the owning control that a row changed.</summary>
    /// <param name="Item">Changed collection item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a precondition collection.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds a new row.</summary>
    /// <returns>The added row.</returns>
    function Add: TOBDPreconditionItem;
    /// <summary>Copies another precondition collection.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>Typed row indexer.</summary>
    property Items[Index: Integer]: TOBDPreconditionItem read GetItem
      write SetItem; default;
    /// <summary>Fires when any row changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>OBD Studio Pro precondition card.</summary>
  TOBDPreconditionList = class(TOBDCustomControl, IOBDSurface)
  strict private
    FItems: TOBDPreconditionCollection;
    FTitle: string;
    FFooterText: string;
    FUpdateDepth: Integer;
    FOnChange: TNotifyEvent;
    procedure SetItems(AValue: TOBDPreconditionCollection);
    procedure SetTitle(const AValue: string);
    procedure SetFooterText(const AValue: string);
    procedure ItemsChanged(Sender: TObject);
    procedure DoChange;
    function UsePreviewData: Boolean;
    function VisualCount: Integer;
    function VisualState(AIndex: Integer): TOBDPreconditionState;
    function VisualLabel(AIndex: Integer): string;
    function VisualValue(AIndex: Integer): string;
    function GetAllMet: Boolean;
    function GetMetCount: Integer;
    function GetRequiredCount: Integer;
    function RowHeight: Integer;
    function HeaderHeight: Integer;
    function FooterHeight: Integer;
    function StateColor(var P: TOBDPainter; AState: TOBDPreconditionState)
      : TColor;
    procedure PaintHeader(var P: TOBDPainter; const R: TRect);
    procedure PaintStateMark(var P: TOBDPainter; AState: TOBDPreconditionState;
      CX, CY: Integer);
    procedure PaintRows(var P: TOBDPainter; const R: TRect);
    procedure PaintFooter(var P: TOBDPainter; const R: TRect);
  protected
    /// <summary>Paints the precondition card.</summary>
    /// <param name="ACanvas">Canvas to paint to.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    /// <summary>Creates the precondition list with preview-sized defaults.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees the row collection.</summary>
    destructor Destroy; override;
    /// <summary>Repaints the card after density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card face used by child controls.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Suspends row change notifications.</summary>
    procedure BeginUpdate;
    /// <summary>Resumes notifications and repaints if rows changed.</summary>
    procedure EndUpdate;
    /// <summary>Removes all precondition rows.</summary>
    procedure Clear;
    /// <summary>Adds and initialises a precondition row.</summary>
    /// <param name="AState">Initial state.</param>
    /// <param name="ALabel">Condition label.</param>
    /// <param name="AValue">Live value text.</param>
    /// <returns>The added row.</returns>
    function AddPrecondition(AState: TOBDPreconditionState;
      const ALabel, AValue: string): TOBDPreconditionItem;
    /// <summary>True when every required row is met.</summary>
    property AllMet: Boolean read GetAllMet;
    /// <summary>Number of required rows that are met.</summary>
    property MetCount: Integer read GetMetCount;
    /// <summary>Number of rows that are required.</summary>
    property RequiredCount: Integer read GetRequiredCount;
  published
    /// <summary>Rows shown inside the card.</summary>
    property Items: TOBDPreconditionCollection read FItems write SetItems;
    /// <summary>Header title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Footer note under the rows.</summary>
    property FooterText: string read FFooterText write SetFooterText;
    /// <summary>Desktop or tablet density.</summary>
    property Density;
    /// <summary>Takes density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires after row data changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

const
  PREVIEW_COUNT = 6;

function PreviewState(AIndex: Integer): TOBDPreconditionState;
begin
  case AIndex of
    0, 1, 2:
      Result := pcnMet;
    3:
      Result := pcnFailed;
    4:
      Result := pcnPending;
  else
    Result := pcnNotRequired;
  end;
end;

function PreviewLabel(AIndex: Integer): string;
begin
  case AIndex of
    0:
      Result := 'Ignition on';
    1:
      Result := 'Engine off';
    2:
      Result := 'Battery voltage ' + #$2265 + ' 12.5 V';
    3:
      Result := 'Battery charger connected';
    4:
      Result := 'Security access';
  else
    Result := 'Doors closed';
  end;
end;

function PreviewValue(AIndex: Integer): string;
begin
  case AIndex of
    0:
      Result := 'Terminal 15';
    1:
      Result := '0 rpm';
    2:
      Result := '12.8 V';
    3:
      Result := 'Not detected';
    4:
      Result := 'Unlocking' + #$2026;
  else
    Result := 'Not required';
  end;
end;

{ TOBDPreconditionItem }

constructor TOBDPreconditionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FState := pcnPending;
end;

procedure TOBDPreconditionItem.Assign(Source: TPersistent);
var
  Item: TOBDPreconditionItem;
begin
  if Source is TOBDPreconditionItem then
  begin
    Item := TOBDPreconditionItem(Source);
    FState := Item.State;
    FLabelText := Item.LabelText;
    FValueText := Item.ValueText;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

function TOBDPreconditionItem.GetDisplayName: string;
begin
  Result := FLabelText;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDPreconditionItem.SetState(AValue: TOBDPreconditionState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  Changed(False);
end;

procedure TOBDPreconditionItem.SetLabelText(const AValue: string);
begin
  if FLabelText = AValue then
    Exit;
  FLabelText := AValue;
  Changed(False);
end;

procedure TOBDPreconditionItem.SetValueText(const AValue: string);
begin
  if FValueText = AValue then
    Exit;
  FValueText := AValue;
  Changed(False);
end;

{ TOBDPreconditionCollection }

constructor TOBDPreconditionCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDPreconditionItem);
end;

function TOBDPreconditionCollection.Add: TOBDPreconditionItem;
begin
  Result := TOBDPreconditionItem(inherited Add);
end;

procedure TOBDPreconditionCollection.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TOBDPreconditionCollection.GetItem(Index: Integer)
  : TOBDPreconditionItem;
begin
  Result := TOBDPreconditionItem(inherited GetItem(Index));
end;

procedure TOBDPreconditionCollection.SetItem(Index: Integer;
  AValue: TOBDPreconditionItem);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDPreconditionCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDPreconditionList }

constructor TOBDPreconditionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle := 'Before you start';
  FFooterText := 'Start becomes available when every condition is met.';
  FItems := TOBDPreconditionCollection.Create(Self);
  FItems.OnChange := ItemsChanged;
  Width := 520;
  Height := HeaderHeight + FooterHeight + RowHeight * PREVIEW_COUNT;
end;

destructor TOBDPreconditionList.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TOBDPreconditionList.DensityChanged;
begin
  inherited DensityChanged;
  Invalidate;
end;

function TOBDPreconditionList.SurfaceColor: TColor;
begin
  if StyleBackground <> clDefault then
    Result := StyleBackground
  else
    Result := Palette.GaugeFace;
end;

procedure TOBDPreconditionList.BeginUpdate;
begin
  Inc(FUpdateDepth);
  FItems.BeginUpdate;
end;

procedure TOBDPreconditionList.EndUpdate;
begin
  FItems.EndUpdate;
  if FUpdateDepth > 0 then
    Dec(FUpdateDepth);
  if FUpdateDepth = 0 then
    DoChange;
end;

procedure TOBDPreconditionList.Clear;
begin
  FItems.Clear;
end;

function TOBDPreconditionList.AddPrecondition(AState: TOBDPreconditionState;
  const ALabel, AValue: string): TOBDPreconditionItem;
begin
  BeginUpdate;
  try
    Result := FItems.Add;
    Result.State := AState;
    Result.LabelText := ALabel;
    Result.ValueText := AValue;
  finally
    EndUpdate;
  end;
end;

procedure TOBDPreconditionList.DoChange;
begin
  if FUpdateDepth > 0 then
    Exit;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TOBDPreconditionList.ItemsChanged(Sender: TObject);
begin
  DoChange;
end;

procedure TOBDPreconditionList.SetItems(AValue: TOBDPreconditionCollection);
begin
  FItems.Assign(AValue);
end;

procedure TOBDPreconditionList.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

procedure TOBDPreconditionList.SetFooterText(const AValue: string);
begin
  if FFooterText = AValue then
    Exit;
  FFooterText := AValue;
  Invalidate;
end;

function TOBDPreconditionList.UsePreviewData: Boolean;
begin
  Result := IsPreview and (FItems.Count = 0);
end;

function TOBDPreconditionList.VisualCount: Integer;
begin
  if UsePreviewData then
    Result := PREVIEW_COUNT
  else
    Result := FItems.Count;
end;

function TOBDPreconditionList.VisualState(AIndex: Integer)
  : TOBDPreconditionState;
begin
  if UsePreviewData then
    Result := PreviewState(AIndex)
  else
    Result := FItems[AIndex].State;
end;

function TOBDPreconditionList.VisualLabel(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewLabel(AIndex)
  else
    Result := FItems[AIndex].LabelText;
end;

function TOBDPreconditionList.VisualValue(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := PreviewValue(AIndex)
  else
    Result := FItems[AIndex].ValueText;
end;

function TOBDPreconditionList.RowHeight: Integer;
begin
  Result := ScaleValue(Metrics.Row - 6);
end;

function TOBDPreconditionList.HeaderHeight: Integer;
begin
  Result := ScaleValue(Metrics.Head);
end;

function TOBDPreconditionList.FooterHeight: Integer;
begin
  Result := ScaleValue(Metrics.Foot);
end;

function TOBDPreconditionList.GetRequiredCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].State <> pcnNotRequired then
      Inc(Result);
end;

function TOBDPreconditionList.GetMetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FItems.Count - 1 do
    if FItems[I].State = pcnMet then
      Inc(Result);
end;

function TOBDPreconditionList.GetAllMet: Boolean;
begin
  Result := MetCount = RequiredCount;
end;

function TOBDPreconditionList.StateColor(var P: TOBDPainter;
  AState: TOBDPreconditionState): TColor;
begin
  case AState of
    pcnMet:
      Result := Palette.Success;
    pcnFailed:
      Result := Palette.Danger;
    pcnPending:
      Result := P.AccentText;
  else
    Result := Palette.Subtle;
  end;
end;

procedure TOBDPreconditionList.PaintHeader(var P: TOBDPainter; const R: TRect);
var
  TextW, ChipW, X, Y, I, Met, Req: Integer;
  ChipText: string;
  ChipColor: TColor;
begin
  X := R.Left + P.S(16);
  Y := R.Top + R.Height div 2;
  TextW := P.Text(X, Y, FTitle, OBD_TEXT_TITLE, Palette.ForegroundText,
    twBold, taLeftJustify, R.Width - P.S(120));
  if UsePreviewData then
  begin
    Met := 0;
    Req := 0;
    for I := 0 to VisualCount - 1 do
      if VisualState(I) <> pcnNotRequired then
      begin
        Inc(Req);
        if VisualState(I) = pcnMet then
          Inc(Met);
      end;
  end
  else
  begin
    Met := MetCount;
    Req := RequiredCount;
  end;
  ChipText := Format('%d OF %d MET', [Met, Req]);
  if Met = Req then
    ChipColor := Palette.Success
  else
    ChipColor := Palette.Warning;
  ChipW := P.ChipWidth(ChipText);
  if X + TextW + P.S(14) + ChipW < R.Right - P.S(16) then
    P.Chip(X + TextW + P.S(14), Y - P.S(10), ChipText, ChipColor);
  P.HLine(R.Left + P.S(1), R.Bottom - P.S(1), R.Width - P.S(2),
    Palette.NeutralLight);
end;

procedure TOBDPreconditionList.PaintStateMark(var P: TOBDPainter;
  AState: TOBDPreconditionState; CX, CY: Integer);
var
  D: Single;
begin
  case AState of
    pcnMet:
      P.GlyphCheck(CX, CY, Palette.Success, 0.9);
    pcnFailed:
      begin
        D := P.SF(5);
        P.Lines([OBDPointF(CX - D, CY - D), OBDPointF(CX + D, CY + D)],
          Palette.Danger, P.SF(2));
        P.Lines([OBDPointF(CX + D, CY - D), OBDPointF(CX - D, CY + D)],
          Palette.Danger, P.SF(2));
      end;
    pcnPending:
      P.GlyphPending(CX, CY, P.AccentText, 0.85);
  else
    P.GlyphDash(CX, CY, Palette.Subtle, 0.8);
  end;
end;

procedure TOBDPreconditionList.PaintRows(var P: TOBDPainter; const R: TRect);
var
  I, Count, Y, RH, Mid, RightW, MaxW: Integer;
  State: TOBDPreconditionState;
  Ink, ValueInk: TColor;
  RowRect: TRect;
begin
  Count := VisualCount;
  RH := RowHeight;
  if Count = 0 then
  begin
    P.Text(R.Left + R.Width div 2, R.Top + R.Height div 2,
      'No preconditions', OBD_TEXT_BODY, Palette.GaugeLabel, twRegular,
      taCenter);
    Exit;
  end;
  Y := R.Top;
  RightW := P.S(150);
  for I := 0 to Count - 1 do
  begin
    if Y >= R.Bottom then
      Break;
    State := VisualState(I);
    RowRect := Rect(R.Left, Y, R.Right, Y + RH);
    if State = pcnFailed then
    begin
      P.FillRect(Rect(RowRect.Left + P.S(1), RowRect.Top, RowRect.Right - P.S(1),
        RowRect.Bottom), P.Tint(Palette.Danger, 0.10));
      P.FillRect(Rect(RowRect.Left + P.S(1), RowRect.Top + P.S(6),
        RowRect.Left + P.S(5), RowRect.Bottom - P.S(6)), Palette.Danger);
    end;
    Mid := RowRect.Top + RH div 2;
    PaintStateMark(P, State, R.Left + P.S(26), Mid);
    if State = pcnNotRequired then
      Ink := Palette.GaugeLabel
    else
      Ink := Palette.ForegroundText;
    ValueInk := StateColor(P, State);
    if State = pcnMet then
      ValueInk := Palette.ForegroundText;
    MaxW := R.Width - P.S(62) - RightW;
    P.Text(R.Left + P.S(46), Mid, VisualLabel(I), OBD_TEXT_BODY, Ink,
      twRegular, taLeftJustify, MaxW);
    P.Text(R.Right - P.S(16), Mid, VisualValue(I), OBD_TEXT_BODY, ValueInk,
      twSemibold, taRightJustify, RightW);
    P.HLine(R.Left + P.S(1), RowRect.Bottom - P.S(1), R.Width - P.S(2),
      Palette.NeutralLight);
    Inc(Y, RH);
  end;
end;

procedure TOBDPreconditionList.PaintFooter(var P: TOBDPainter; const R: TRect);
begin
  P.HLine(R.Left + P.S(1), R.Top, R.Width - P.S(2), Palette.NeutralLight);
  P.Text(R.Left + P.S(16), R.Top + R.Height div 2, FFooterText,
    OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taLeftJustify,
    R.Width - P.S(32));
end;

procedure TOBDPreconditionList.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  HR, RR, FR: TRect;
  H, F: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(Rect(0, 0, Width, Height), clNone, SurfaceColor);
    H := HeaderHeight;
    F := FooterHeight;
    HR := Rect(0, 0, Width, H);
    FR := Rect(0, Height - F, Width, Height);
    RR := Rect(0, H, Width, FR.Top);
    PaintHeader(P, HR);
    PaintRows(P, RR);
    PaintFooter(P, FR);
  finally
    P.Free;
  end;
end;

end.
