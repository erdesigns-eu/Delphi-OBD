//------------------------------------------------------------------------------
//  ERD.UI.ProcedureView
//
//  TOBDProcedureView - guided procedure card with step progress, instructions,
//  illustration, live values and footer actions for OBD Studio Pro workflows.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------

unit ERD.UI.ProcedureView;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Types,
  System.UITypes,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint;

type
  /// <summary>State colour used by a live value in the procedure side column.</summary>
  TOBDProcedureValueState = (
    /// <summary>Plain text without status emphasis.</summary>
    opvNeutral,
    /// <summary>Green success value.</summary>
    opvSuccess,
    /// <summary>Accent in-progress value.</summary>
    opvAccent,
    /// <summary>Amber warning value.</summary>
    opvWarning,
    /// <summary>Red danger value.</summary>
    opvDanger);

  TOBDProcedureView = class;

  /// <summary>One catalogue-driven instruction step shown by <see cref="TOBDProcedureView"/>.</summary>
  TOBDProcedureStep = class(TCollectionItem)
  strict private
    FCaption: string;
    FTitle: string;
    FInstructions: TStringList;
    FPictureCaption: string;
    FGlyph: TOBDGlyph;
    FPicture: TPicture;
    FWarningTitle: string;
    FWarningText: string;
    FActionCaption: string;
    procedure SetCaption(const AValue: string);
    procedure SetTitle(const AValue: string);
    procedure SetInstructions(AValue: TStrings);
    procedure SetPictureCaption(const AValue: string);
    procedure SetGlyph(AValue: TOBDGlyph);
    procedure SetPicture(AValue: TPicture);
    procedure SetWarningTitle(const AValue: string);
    procedure SetWarningText(const AValue: string);
    procedure SetActionCaption(const AValue: string);
    procedure StringsChanged(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure DoChanged;
  protected
    /// <summary>Returns the caption used by the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates a procedure step with a routine glyph.</summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>Frees owned instructions and picture objects.</summary>
    destructor Destroy; override;
    /// <summary>Copies another step item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Short label displayed in the step strip.</summary>
    property Caption: string read FCaption write SetCaption;
    /// <summary>Heading of the current step.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Numbered instruction lines wrapped in the body.</summary>
    property Instructions: TStrings read FInstructions write SetInstructions;
    /// <summary>Caption displayed under the optional picture.</summary>
    property PictureCaption: string read FPictureCaption write SetPictureCaption;
    /// <summary>Built-in glyph used by the step illustration placeholder.</summary>
    property Glyph: TOBDGlyph read FGlyph write SetGlyph default glRoutine;
    /// <summary>Optional bitmap or graphic supplied by the host application.</summary>
    property Picture: TPicture read FPicture write SetPicture;
    /// <summary>Optional warning banner title for the current step.</summary>
    property WarningTitle: string read FWarningTitle write SetWarningTitle;
    /// <summary>Optional warning banner body text for the current step.</summary>
    property WarningText: string read FWarningText write SetWarningText;
    /// <summary>Caption of the step-specific action button.</summary>
    property ActionCaption: string read FActionCaption write SetActionCaption;
  end;

  /// <summary>Owned collection of procedure steps.</summary>
  TOBDProcedureStepCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDProcedureStep;
    procedure SetItem(Index: Integer; AValue: TOBDProcedureStep);
  protected
    /// <summary>Notifies the owner when a step item changes.</summary>
    /// <param name="Item">Changed collection item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a step collection owned by the procedure view.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds one step item.</summary>
    /// <returns>New step.</returns>
    function Add: TOBDProcedureStep;
    /// <summary>Indexed step items.</summary>
    property Items[Index: Integer]: TOBDProcedureStep read GetItem write SetItem; default;
    /// <summary>Fires whenever the collection changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>One live value displayed beside the current procedure step.</summary>
  TOBDProcedureLiveValue = class(TCollectionItem)
  strict private
    FLabelText: string;
    FValueText: string;
    FState: TOBDProcedureValueState;
    FFraction: Double;
    FShowBar: Boolean;
    procedure SetLabelText(const AValue: string);
    procedure SetValueText(const AValue: string);
    procedure SetState(AValue: TOBDProcedureValueState);
    procedure SetFraction(AValue: Double);
    procedure SetShowBar(AValue: Boolean);
    procedure DoChanged;
  protected
    /// <summary>Returns the label used by the collection editor.</summary>
    function GetDisplayName: string; override;
  public
    /// <summary>Creates an empty live value row.</summary>
    constructor Create(Collection: TCollection); override;
    /// <summary>Copies another live value item.</summary>
    /// <param name="Source">Source persistent.</param>
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>Label on the left of the value row.</summary>
    property LabelText: string read FLabelText write SetLabelText;
    /// <summary>Value text right-aligned in the row.</summary>
    property ValueText: string read FValueText write SetValueText;
    /// <summary>State colour of the value and optional bar.</summary>
    property State: TOBDProcedureValueState read FState write SetState default opvNeutral;
    /// <summary>Progress fraction for the optional bar, clamped to 0..1.</summary>
    property Fraction: Double read FFraction write SetFraction;
    /// <summary>Whether the fraction bar is painted below this row.</summary>
    property ShowBar: Boolean read FShowBar write SetShowBar default False;
  end;

  /// <summary>Owned collection of procedure live values.</summary>
  TOBDProcedureLiveValueCollection = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TOBDProcedureLiveValue;
    procedure SetItem(Index: Integer; AValue: TOBDProcedureLiveValue);
  protected
    /// <summary>Notifies the owner when a live value changes.</summary>
    /// <param name="Item">Changed collection item.</param>
    procedure Update(Item: TCollectionItem); override;
  public
    /// <summary>Creates a live-value collection owned by the procedure view.</summary>
    /// <param name="AOwner">Persistent owner.</param>
    constructor Create(AOwner: TPersistent);
    /// <summary>Adds one live value row.</summary>
    /// <returns>New live value.</returns>
    function Add: TOBDProcedureLiveValue;
    /// <summary>Indexed live values.</summary>
    property Items[Index: Integer]: TOBDProcedureLiveValue read GetItem write SetItem; default;
    /// <summary>Fires whenever the collection changes.</summary>
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  /// <summary>Step-change event.</summary>
  TOBDProcedureStepChangeEvent = procedure(Sender: TObject; AIndex: Integer) of object;

  /// <summary>Guided OBD Studio Pro procedure card.</summary>
  TOBDProcedureView = class(TOBDCustomControl, IOBDSurface)
  strict private
    FTitle: string;
    FSubtitle: string;
    FGlyph: TOBDGlyph;
    FSteps: TOBDProcedureStepCollection;
    FLiveValues: TOBDProcedureLiveValueCollection;
    FCurrentStep: Integer;
    FNextEnabled: Boolean;
    FSourceText: string;
    FUpdateCount: Integer;
    FHotButton: Integer;
    FDownButton: Integer;
    FHotStep: Integer;
    FOnAbort: TNotifyEvent;
    FOnBack: TNotifyEvent;
    FOnNext: TNotifyEvent;
    FOnAction: TNotifyEvent;
    FOnStepChange: TOBDProcedureStepChangeEvent;
    procedure SetTitle(const AValue: string);
    procedure SetSubtitle(const AValue: string);
    procedure SetGlyph(AValue: TOBDGlyph);
    procedure SetSteps(AValue: TOBDProcedureStepCollection);
    procedure SetLiveValues(AValue: TOBDProcedureLiveValueCollection);
    procedure SetCurrentStep(AValue: Integer);
    procedure SetNextEnabled(AValue: Boolean);
    procedure SetSourceText(const AValue: string);
    procedure ModelChanged(Sender: TObject);
    procedure DoChanged;
    procedure DoStepChange;
    function UsePreviewData: Boolean;
    function StepCount: Integer;
    function VisualStepCaption(AIndex: Integer): string;
    function VisualStepTitle(AIndex: Integer): string;
    function VisualStepInstructionCount(AIndex: Integer): Integer;
    function VisualStepInstruction(AIndex, ALine: Integer): string;
    function VisualStepGlyph(AIndex: Integer): TOBDGlyph;
    function VisualStepPictureCaption(AIndex: Integer): string;
    function VisualStepWarningTitle(AIndex: Integer): string;
    function VisualStepWarningText(AIndex: Integer): string;
    function VisualStepActionCaption(AIndex: Integer): string;
    function VisualLiveCount: Integer;
    function VisualLiveLabel(AIndex: Integer): string;
    function VisualLiveValue(AIndex: Integer): string;
    function VisualLiveState(AIndex: Integer): TOBDProcedureValueState;
    function VisualLiveFraction(AIndex: Integer): Double;
    function VisualLiveShowBar(AIndex: Integer): Boolean;
    function VisualCurrentStep: Integer;
    function ButtonRect(AButton: Integer; APainter: TOBDPainter): TRect;
    function FooterRect: TRect;
    function StepStripRect: TRect;
    function HitButton(X, Y: Integer; APainter: TOBDPainter): Integer;
    function HitStep(X, Y: Integer): Integer;
    function ButtonState(AButton: Integer): TOBDControlState;
    function StateColor(APainter: TOBDPainter; AState: TOBDProcedureValueState): TColor;
    procedure PaintHeader(APainter: TOBDPainter);
    procedure PaintStepStrip(APainter: TOBDPainter);
    procedure PaintBody(APainter: TOBDPainter; ACanvas: TCanvas);
    procedure PaintFooter(APainter: TOBDPainter);
    procedure PaintEmpty(APainter: TOBDPainter);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    /// <summary>Paints the procedure card.</summary>
    /// <param name="ACanvas">Target canvas.</param>
    procedure PaintControl(ACanvas: TCanvas); override;
    /// <summary>Handles footer button and step-strip clicks.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Tracks hover state for buttons and steps.</summary>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Executes a pressed footer button.</summary>
    /// <param name="Button">Mouse button.</param>
    /// <param name="Shift">Modifier keys.</param>
    /// <param name="X">Client X.</param>
    /// <param name="Y">Client Y.</param>
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    /// <summary>Handles keyboard navigation between steps.</summary>
    /// <param name="Key">Virtual key.</param>
    /// <param name="Shift">Modifier keys.</param>
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    /// <summary>Creates the procedure view and its model collections.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Frees owned collections.</summary>
    destructor Destroy; override;
    /// <summary>Repaints after density changes.</summary>
    procedure DensityChanged; override;
    /// <summary>Returns the card surface colour for child controls.</summary>
    /// <returns>Surface colour.</returns>
    function SurfaceColor: TColor;
    /// <summary>Suspends repaint/change notifications.</summary>
    procedure BeginUpdate;
    /// <summary>Resumes repaint/change notifications.</summary>
    procedure EndUpdate;
    /// <summary>Removes all steps and live values.</summary>
    procedure Clear;
    /// <summary>Adds one procedure step.</summary>
    /// <param name="ACaption">Step-strip caption.</param>
    /// <param name="ATitle">Step heading.</param>
    /// <returns>New step.</returns>
    function AddStep(const ACaption, ATitle: string): TOBDProcedureStep;
    /// <summary>Adds one live value row.</summary>
    /// <param name="ALabel">Row label.</param>
    /// <param name="AValue">Row value.</param>
    /// <param name="AState">State colour.</param>
    /// <param name="AFraction">Optional fraction.</param>
    /// <returns>New live value.</returns>
    function AddLiveValue(const ALabel, AValue: string; AState: TOBDProcedureValueState = opvNeutral; AFraction: Double = -1): TOBDProcedureLiveValue;
  published
    /// <summary>Header title.</summary>
    property Title: string read FTitle write SetTitle;
    /// <summary>Header subtitle, usually ECU and catalogue source.</summary>
    property Subtitle: string read FSubtitle write SetSubtitle;
    /// <summary>Header glyph.</summary>
    property Glyph: TOBDGlyph read FGlyph write SetGlyph default glRoutine;
    /// <summary>Procedure steps loaded by the host from a catalogue file.</summary>
    property Steps: TOBDProcedureStepCollection read FSteps write SetSteps;
    /// <summary>Live values displayed in the side column.</summary>
    property LiveValues: TOBDProcedureLiveValueCollection read FLiveValues write SetLiveValues;
    /// <summary>Current zero-based step index.</summary>
    property CurrentStep: Integer read FCurrentStep write SetCurrentStep default 0;
    /// <summary>Whether the Next button is enabled.</summary>
    property NextEnabled: Boolean read FNextEnabled write SetNextEnabled default True;
    /// <summary>Footer source text for catalogue provenance.</summary>
    property SourceText: string read FSourceText write SetSourceText;
    /// <summary>Desktop or tablet sizes.</summary>
    property Density;
    /// <summary>Takes the density from the theme.</summary>
    property ParentDensity;
    /// <summary>Fires when Abort is clicked.</summary>
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    /// <summary>Fires when Back is clicked.</summary>
    property OnBack: TNotifyEvent read FOnBack write FOnBack;
    /// <summary>Fires when Next is clicked.</summary>
    property OnNext: TNotifyEvent read FOnNext write FOnNext;
    /// <summary>Fires when the step action is clicked.</summary>
    property OnAction: TNotifyEvent read FOnAction write FOnAction;
    /// <summary>Fires after <see cref="CurrentStep"/> changes.</summary>
    property OnStepChange: TOBDProcedureStepChangeEvent read FOnStepChange write FOnStepChange;
  end;

implementation

const
  BTN_NONE = 0;
  BTN_ABORT = 1;
  BTN_BACK = 2;
  BTN_ACTION = 3;
  BTN_NEXT = 4;

{ TOBDProcedureStep --------------------------------------------------------- }

constructor TOBDProcedureStep.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FInstructions := TStringList.Create;
  FInstructions.OnChange := StringsChanged;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FGlyph := glRoutine;
end;

destructor TOBDProcedureStep.Destroy;
begin
  FPicture.Free;
  FInstructions.Free;
  inherited;
end;

procedure TOBDProcedureStep.Assign(Source: TPersistent);
var
  Step: TOBDProcedureStep;
begin
  if Source is TOBDProcedureStep then
  begin
    Step := TOBDProcedureStep(Source);
    FCaption := Step.Caption;
    FTitle := Step.Title;
    FInstructions.Assign(Step.Instructions);
    FPictureCaption := Step.PictureCaption;
    FGlyph := Step.Glyph;
    FPicture.Assign(Step.Picture);
    FWarningTitle := Step.WarningTitle;
    FWarningText := Step.WarningText;
    FActionCaption := Step.ActionCaption;
    DoChanged;
  end
  else
    inherited Assign(Source);
end;

procedure TOBDProcedureStep.DoChanged;
begin
  Changed(False);
end;

function TOBDProcedureStep.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := FTitle;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDProcedureStep.PictureChanged(Sender: TObject);
begin
  DoChanged;
end;

procedure TOBDProcedureStep.SetActionCaption(const AValue: string);
begin
  if FActionCaption = AValue then
    Exit;
  FActionCaption := AValue;
  DoChanged;
end;

procedure TOBDProcedureStep.SetCaption(const AValue: string);
begin
  if FCaption = AValue then
    Exit;
  FCaption := AValue;
  DoChanged;
end;

procedure TOBDProcedureStep.SetGlyph(AValue: TOBDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  DoChanged;
end;

procedure TOBDProcedureStep.SetInstructions(AValue: TStrings);
begin
  FInstructions.Assign(AValue);
  DoChanged;
end;

procedure TOBDProcedureStep.SetPicture(AValue: TPicture);
begin
  FPicture.Assign(AValue);
  DoChanged;
end;

procedure TOBDProcedureStep.SetPictureCaption(const AValue: string);
begin
  if FPictureCaption = AValue then
    Exit;
  FPictureCaption := AValue;
  DoChanged;
end;

procedure TOBDProcedureStep.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  DoChanged;
end;

procedure TOBDProcedureStep.SetWarningText(const AValue: string);
begin
  if FWarningText = AValue then
    Exit;
  FWarningText := AValue;
  DoChanged;
end;

procedure TOBDProcedureStep.SetWarningTitle(const AValue: string);
begin
  if FWarningTitle = AValue then
    Exit;
  FWarningTitle := AValue;
  DoChanged;
end;

procedure TOBDProcedureStep.StringsChanged(Sender: TObject);
begin
  DoChanged;
end;

{ TOBDProcedureStepCollection ---------------------------------------------- }

constructor TOBDProcedureStepCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDProcedureStep);
end;

function TOBDProcedureStepCollection.Add: TOBDProcedureStep;
begin
  Result := TOBDProcedureStep(inherited Add);
end;

function TOBDProcedureStepCollection.GetItem(Index: Integer): TOBDProcedureStep;
begin
  Result := TOBDProcedureStep(inherited GetItem(Index));
end;

procedure TOBDProcedureStepCollection.SetItem(Index: Integer; AValue: TOBDProcedureStep);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDProcedureStepCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDProcedureLiveValue ---------------------------------------------------- }

constructor TOBDProcedureLiveValue.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFraction := -1;
end;

procedure TOBDProcedureLiveValue.Assign(Source: TPersistent);
var
  Value: TOBDProcedureLiveValue;
begin
  if Source is TOBDProcedureLiveValue then
  begin
    Value := TOBDProcedureLiveValue(Source);
    FLabelText := Value.LabelText;
    FValueText := Value.ValueText;
    FState := Value.State;
    FFraction := Value.Fraction;
    FShowBar := Value.ShowBar;
    DoChanged;
  end
  else
    inherited Assign(Source);
end;

procedure TOBDProcedureLiveValue.DoChanged;
begin
  Changed(False);
end;

function TOBDProcedureLiveValue.GetDisplayName: string;
begin
  Result := FLabelText;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TOBDProcedureLiveValue.SetFraction(AValue: Double);
begin
  if AValue >= 0 then
    AValue := EnsureRange(AValue, 0, 1);
  if SameValue(FFraction, AValue) then
    Exit;
  FFraction := AValue;
  if AValue >= 0 then
    FShowBar := True;
  DoChanged;
end;

procedure TOBDProcedureLiveValue.SetLabelText(const AValue: string);
begin
  if FLabelText = AValue then
    Exit;
  FLabelText := AValue;
  DoChanged;
end;

procedure TOBDProcedureLiveValue.SetShowBar(AValue: Boolean);
begin
  if FShowBar = AValue then
    Exit;
  FShowBar := AValue;
  DoChanged;
end;

procedure TOBDProcedureLiveValue.SetState(AValue: TOBDProcedureValueState);
begin
  if FState = AValue then
    Exit;
  FState := AValue;
  DoChanged;
end;

procedure TOBDProcedureLiveValue.SetValueText(const AValue: string);
begin
  if FValueText = AValue then
    Exit;
  FValueText := AValue;
  DoChanged;
end;

{ TOBDProcedureLiveValueCollection ----------------------------------------- }

constructor TOBDProcedureLiveValueCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TOBDProcedureLiveValue);
end;

function TOBDProcedureLiveValueCollection.Add: TOBDProcedureLiveValue;
begin
  Result := TOBDProcedureLiveValue(inherited Add);
end;

function TOBDProcedureLiveValueCollection.GetItem(Index: Integer): TOBDProcedureLiveValue;
begin
  Result := TOBDProcedureLiveValue(inherited GetItem(Index));
end;

procedure TOBDProcedureLiveValueCollection.SetItem(Index: Integer; AValue: TOBDProcedureLiveValue);
begin
  inherited SetItem(Index, AValue);
end;

procedure TOBDProcedureLiveValueCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TOBDProcedureView --------------------------------------------------------- }

constructor TOBDProcedureView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;
  Width := 760;
  Height := 430;
  FTitle := 'Procedure';
  FSubtitle := '';
  FGlyph := glRoutine;
  FNextEnabled := True;
  FHotButton := BTN_NONE;
  FDownButton := BTN_NONE;
  FHotStep := -1;
  FSteps := TOBDProcedureStepCollection.Create(Self);
  FSteps.OnChange := ModelChanged;
  FLiveValues := TOBDProcedureLiveValueCollection.Create(Self);
  FLiveValues.OnChange := ModelChanged;
end;

destructor TOBDProcedureView.Destroy;
begin
  FLiveValues.Free;
  FSteps.Free;
  inherited;
end;

procedure TOBDProcedureView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TOBDProcedureView.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    DoChanged;
end;

procedure TOBDProcedureView.Clear;
begin
  BeginUpdate;
  try
    FSteps.Clear;
    FLiveValues.Clear;
    FCurrentStep := 0;
  finally
    EndUpdate;
  end;
end;

function TOBDProcedureView.AddStep(const ACaption, ATitle: string): TOBDProcedureStep;
begin
  Result := FSteps.Add;
  Result.Caption := ACaption;
  Result.Title := ATitle;
  DoChanged;
end;

function TOBDProcedureView.AddLiveValue(const ALabel, AValue: string; AState: TOBDProcedureValueState; AFraction: Double): TOBDProcedureLiveValue;
begin
  Result := FLiveValues.Add;
  Result.LabelText := ALabel;
  Result.ValueText := AValue;
  Result.State := AState;
  if AFraction >= 0 then
    Result.Fraction := AFraction;
  DoChanged;
end;

function TOBDProcedureView.SurfaceColor: TColor;
begin
  Result := Palette.GaugeFace;
end;

procedure TOBDProcedureView.DensityChanged;
begin
  inherited;
  Invalidate;
end;

procedure TOBDProcedureView.DoChanged;
begin
  if FUpdateCount > 0 then
    Exit;
  if FCurrentStep >= StepCount then
    FCurrentStep := System.Math.Max(0, StepCount - 1);
  Invalidate;
end;

procedure TOBDProcedureView.DoStepChange;
begin
  if Assigned(FOnStepChange) then
    FOnStepChange(Self, FCurrentStep);
end;

procedure TOBDProcedureView.ModelChanged(Sender: TObject);
begin
  DoChanged;
end;

procedure TOBDProcedureView.SetCurrentStep(AValue: Integer);
var
  MaxIndex: Integer;
begin
  MaxIndex := System.Math.Max(0, StepCount - 1);
  AValue := EnsureRange(AValue, 0, MaxIndex);
  if FCurrentStep = AValue then
    Exit;
  FCurrentStep := AValue;
  Invalidate;
  DoStepChange;
end;

procedure TOBDProcedureView.SetGlyph(AValue: TOBDGlyph);
begin
  if FGlyph = AValue then
    Exit;
  FGlyph := AValue;
  Invalidate;
end;

procedure TOBDProcedureView.SetLiveValues(AValue: TOBDProcedureLiveValueCollection);
begin
  FLiveValues.Assign(AValue);
  DoChanged;
end;

procedure TOBDProcedureView.SetNextEnabled(AValue: Boolean);
begin
  if FNextEnabled = AValue then
    Exit;
  FNextEnabled := AValue;
  Invalidate;
end;

procedure TOBDProcedureView.SetSourceText(const AValue: string);
begin
  if FSourceText = AValue then
    Exit;
  FSourceText := AValue;
  Invalidate;
end;

procedure TOBDProcedureView.SetSteps(AValue: TOBDProcedureStepCollection);
begin
  FSteps.Assign(AValue);
  DoChanged;
end;

procedure TOBDProcedureView.SetSubtitle(const AValue: string);
begin
  if FSubtitle = AValue then
    Exit;
  FSubtitle := AValue;
  Invalidate;
end;

procedure TOBDProcedureView.SetTitle(const AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  Invalidate;
end;

function TOBDProcedureView.UsePreviewData: Boolean;
begin
  Result := (FSteps.Count = 0) and IsPreview;
end;

function TOBDProcedureView.StepCount: Integer;
begin
  if UsePreviewData then
    Result := 5
  else
    Result := FSteps.Count;
end;

function TOBDProcedureView.VisualCurrentStep: Integer;
begin
  Result := EnsureRange(FCurrentStep, 0, System.Math.Max(0, StepCount - 1));
  if UsePreviewData and (FCurrentStep = 0) then
    Result := 1;
end;

function TOBDProcedureView.VisualStepCaption(AIndex: Integer): string;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := 'Preconditions';
      1: Result := 'Open calipers';
      2: Result := 'Replace pads';
      3: Result := 'Close calipers';
    else
      Result := 'Finish';
    end
  else
    Result := FSteps[AIndex].Caption;
end;

function TOBDProcedureView.VisualStepTitle(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := 'Open the rear calipers'
  else
    Result := FSteps[AIndex].Title;
end;

function TOBDProcedureView.VisualStepInstructionCount(AIndex: Integer): Integer;
begin
  if UsePreviewData then
    Result := 4
  else
    Result := FSteps[AIndex].Instructions.Count;
end;

function TOBDProcedureView.VisualStepInstruction(AIndex, ALine: Integer): string;
begin
  if UsePreviewData then
    case ALine of
      0: Result := 'Press Open calipers. Both motors retract the pistons.';
      1: Result := 'Wait until both sides show Open.';
      2: Result := 'Do not press the brake pedal or the EPB switch.';
    else
      Result := 'Then remove the wheels and continue.';
    end
  else
    Result := FSteps[AIndex].Instructions[ALine];
end;

function TOBDProcedureView.VisualStepGlyph(AIndex: Integer): TOBDGlyph;
begin
  if UsePreviewData then
    Result := glBrake
  else
    Result := FSteps[AIndex].Glyph;
end;

function TOBDProcedureView.VisualStepPictureCaption(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := 'Caliper, rear left'
  else
    Result := FSteps[AIndex].PictureCaption;
end;

function TOBDProcedureView.VisualStepWarningTitle(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := 'Motors run for up to 30 s'
  else
    Result := FSteps[AIndex].WarningTitle;
end;

function TOBDProcedureView.VisualStepWarningText(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := 'Keep hands away from the calipers while they move.'
  else
    Result := FSteps[AIndex].WarningText;
end;

function TOBDProcedureView.VisualStepActionCaption(AIndex: Integer): string;
begin
  if UsePreviewData then
    Result := 'Open calipers'
  else
    Result := FSteps[AIndex].ActionCaption;
end;

function TOBDProcedureView.VisualLiveCount: Integer;
begin
  if UsePreviewData then
    Result := 4
  else
    Result := FLiveValues.Count;
end;

function TOBDProcedureView.VisualLiveLabel(AIndex: Integer): string;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := 'Left motor';
      1: Result := 'Right motor';
      2: Result := 'Motor current';
    else
      Result := 'Battery';
    end
  else
    Result := FLiveValues[AIndex].LabelText;
end;

function TOBDProcedureView.VisualLiveValue(AIndex: Integer): string;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := 'Open';
      1: Result := 'Moving ' + #$00B7 + ' 62 %';
      2: Result := '3.4 A';
    else
      Result := '12.7 V';
    end
  else
    Result := FLiveValues[AIndex].ValueText;
end;

function TOBDProcedureView.VisualLiveState(AIndex: Integer): TOBDProcedureValueState;
begin
  if UsePreviewData then
  begin
    if AIndex = 1 then
      Result := opvAccent
    else
      Result := opvNeutral;
  end
  else
    Result := FLiveValues[AIndex].State;
end;

function TOBDProcedureView.VisualLiveFraction(AIndex: Integer): Double;
begin
  if UsePreviewData then
    case AIndex of
      0: Result := 1.0;
      1: Result := 0.62;
    else
      Result := -1;
    end
  else
    Result := FLiveValues[AIndex].Fraction;
end;

function TOBDProcedureView.VisualLiveShowBar(AIndex: Integer): Boolean;
begin
  if UsePreviewData then
    Result := AIndex in [0, 1]
  else
    Result := FLiveValues[AIndex].ShowBar;
end;

function TOBDProcedureView.FooterRect: TRect;
var
  FH: Integer;
begin
  FH := ScaleValue(Metrics.Foot + 8);
  Result := Rect(0, ClientHeight - FH, ClientWidth, ClientHeight);
end;

function TOBDProcedureView.StepStripRect: TRect;
var
  TopY: Integer;
begin
  TopY := ScaleValue(Metrics.Head + 14);
  Result := Rect(ScaleValue(20), TopY, ClientWidth - ScaleValue(20), TopY + ScaleValue(30));
end;

function TOBDProcedureView.ButtonRect(AButton: Integer; APainter: TOBDPainter): TRect;
var
  F, R: TRect;
  BW, BH, Gap, X, Y: Integer;
  Caption: string;
begin
  Result := Rect(0, 0, 0, 0);
  F := FooterRect;
  BH := ScaleValue(Metrics.Button);
  Gap := ScaleValue(8);
  Y := F.Top + (F.Height - BH) div 2;
  if AButton = BTN_ABORT then
  begin
    BW := APainter.ButtonWidth('Abort', glNone);
    Result := Rect(ScaleValue(16), Y, ScaleValue(16) + BW, Y + BH);
    Exit;
  end;

  R := Rect(0, Y, ClientWidth - ScaleValue(16), Y + BH);
  BW := APainter.ButtonWidth('Next', glNone);
  X := R.Right - BW;
  if AButton = BTN_NEXT then
    Exit(Rect(X, Y, X + BW, Y + BH));
  Dec(X, Gap);
  if StepCount > 0 then
  begin
    Caption := VisualStepActionCaption(VisualCurrentStep);
    if Caption <> '' then
    begin
      BW := APainter.ButtonWidth(Caption, glNone);
      Dec(X, BW);
      if AButton = BTN_ACTION then
        Exit(Rect(X, Y, X + BW, Y + BH));
      Dec(X, Gap);
    end;
  end;
  BW := APainter.ButtonWidth('Back', glNone);
  Dec(X, BW);
  if AButton = BTN_BACK then
    Result := Rect(X, Y, X + BW, Y + BH);
end;

function TOBDProcedureView.HitButton(X, Y: Integer; APainter: TOBDPainter): Integer;
var
  I: Integer;
begin
  Result := BTN_NONE;
  for I := BTN_ABORT to BTN_NEXT do
    if PtInRect(ButtonRect(I, APainter), Point(X, Y)) then
      Exit(I);
end;

function TOBDProcedureView.HitStep(X, Y: Integer): Integer;
var
  R: TRect;
  Count, W: Integer;
begin
  Result := -1;
  Count := StepCount;
  if Count <= 0 then
    Exit;
  R := StepStripRect;
  if not PtInRect(R, Point(X, Y)) then
    Exit;
  W := System.Math.Max(1, R.Width div Count);
  Result := EnsureRange((X - R.Left) div W, 0, Count - 1);
end;

function TOBDProcedureView.ButtonState(AButton: Integer): TOBDControlState;
begin
  Result := cstNormal;
  if (AButton = BTN_BACK) and (VisualCurrentStep <= 0) then
    Result := cstDisabled
  else if (AButton = BTN_NEXT) and ((StepCount = 0) or not FNextEnabled) then
    Result := cstDisabled
  else if FDownButton = AButton then
    Result := cstPressed
  else if FHotButton = AButton then
    Result := cstHover;
end;

function TOBDProcedureView.StateColor(APainter: TOBDPainter; AState: TOBDProcedureValueState): TColor;
begin
  case AState of
    opvSuccess: Result := Palette.Success;
    opvAccent: Result := APainter.AccentText;
    opvWarning: Result := Palette.Warning;
    opvDanger: Result := APainter.DangerInk;
  else
    Result := Palette.ForegroundText;
  end;
end;

procedure TOBDProcedureView.PaintHeader(APainter: TOBDPainter);
var
  H, X, ChipX, ChipW: Integer;
  StateText, HeadTitle, HeadSub: string;
begin
  H := ScaleValue(Metrics.Head);
  X := ScaleValue(16);
  APainter.Glyph(FGlyph, X + ScaleValue(8), H div 2, APainter.AccentText, 1);
  Inc(X, ScaleValue(26));
  HeadTitle := FTitle;
  if UsePreviewData and (HeadTitle = 'Procedure') then
    HeadTitle := 'Electronic parking brake ' + #$00B7 + ' service mode';
  HeadSub := FSubtitle;
  if UsePreviewData and (HeadSub = '') then
    HeadSub := '53 Parking brake ' + #$00B7 + ' Catalogue: VAG MQB (garage copy)';
  APainter.Text(X, H div 2 - ScaleValue(8), HeadTitle, OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold, taLeftJustify, ClientWidth - X - ScaleValue(180));
  APainter.Text(X, H div 2 + ScaleValue(12), HeadSub, OBD_TEXT_SMALL,
    Palette.GaugeLabel, twRegular, taLeftJustify, ClientWidth - X - ScaleValue(32));
  StateText := Format('STEP %d OF %d', [VisualCurrentStep + 1, System.Math.Max(1, StepCount)]);
  ChipW := APainter.ChipWidth(StateText);
  ChipX := ClientWidth - ScaleValue(16) - ChipW;
  APainter.Chip(ChipX, (H - ScaleValue(20)) div 2, StateText, APainter.AccentText);
  APainter.HLine(1, H - 1, ClientWidth - 2, Palette.NeutralLight);
end;

procedure TOBDProcedureView.PaintStepStrip(APainter: TOBDPainter);
var
  R: TRect;
  I, Count, SegmentW, SX, CY: Integer;
  C, TextColor: TColor;
  Weight: TOBDTextWeight;
  Caption: string;
begin
  R := StepStripRect;
  Count := StepCount;
  if Count <= 0 then
    Exit;
  SegmentW := R.Width div Count;
  for I := 0 to Count - 1 do
  begin
    SX := R.Left + I * SegmentW;
    CY := R.Top + ScaleValue(18);
    if I < VisualCurrentStep then
      C := Palette.Success
    else if I = VisualCurrentStep then
      C := Palette.Accent
    else
      C := Palette.NeutralLight;
    APainter.FillRect(Rect(SX, R.Top, SX + SegmentW - ScaleValue(6), R.Top + ScaleValue(4)), C);
    if I < VisualCurrentStep then
      APainter.GlyphCheck(SX + ScaleValue(7), CY, Palette.Success, 0.7)
    else if I = VisualCurrentStep then
      APainter.GlyphPending(SX + ScaleValue(7), CY, APainter.AccentText, 0.75)
    else
      APainter.Text(SX + ScaleValue(7), CY, IntToStr(I + 1), OBD_TEXT_SMALL,
        Palette.GaugeLabel, twBold, taCenter);
    Caption := VisualStepCaption(I);
    if I > VisualCurrentStep then
      TextColor := Palette.GaugeLabel
    else
      TextColor := Palette.ForegroundText;
    if I = VisualCurrentStep then
      Weight := twSemibold
    else
      Weight := twRegular;
    APainter.Text(SX + ScaleValue(18), CY, Caption, OBD_TEXT_BODY,
      TextColor, Weight, taLeftJustify, SegmentW - ScaleValue(28));
  end;
  APainter.HLine(ScaleValue(4), R.Bottom + ScaleValue(12), ClientWidth - ScaleValue(8), Palette.NeutralLight);
end;

procedure TOBDProcedureView.PaintBody(APainter: TOBDPainter; ACanvas: TCanvas);
var
  BodyTop, FootTop, RightW, RX, LW, X, Y, I, N, PicW, PicH, Used, LineH: Integer;
  R, PicR, BannerR, TextR: TRect;
  Step: Integer;
  ValueColor, BarColor: TColor;
  Frac: Double;
  Save: Integer;
  Graphic: TGraphic;
begin
  Step := VisualCurrentStep;
  BodyTop := StepStripRect.Bottom + ScaleValue(28);
  FootTop := FooterRect.Top;
  RightW := ScaleValue(IfThen(ClientWidth > ScaleValue(760), 260, 220));
  RX := ClientWidth - RightW - ScaleValue(20);
  LW := RX - ScaleValue(36);
  X := ScaleValue(20);

  APainter.Caps(X, BodyTop + ScaleValue(6), Format('Step %d of %d', [Step + 1, System.Math.Max(1, StepCount)]));
  APainter.Text(X, BodyTop + ScaleValue(30), VisualStepTitle(Step), OBD_TEXT_TITLE,
    Palette.ForegroundText, twBold, taLeftJustify, LW - X);

  PicW := 0;
  if LW - X > ScaleValue(520) then
    PicW := ScaleValue(180);
  Y := BodyTop + ScaleValue(58);
  N := VisualStepInstructionCount(Step);
  for I := 0 to N - 1 do
  begin
    APainter.Text(X, Y + ScaleValue(8), IntToStr(I + 1) + '.', OBD_TEXT_BODY,
      Palette.GaugeLabel, twSemibold);
    TextR := Rect(X + ScaleValue(24), Y, LW - PicW - ScaleValue(16), FootTop - ScaleValue(12));
    Used := APainter.WrapText(TextR, VisualStepInstruction(Step, I), OBD_TEXT_BODY,
      Palette.ForegroundText);
    LineH := System.Math.Max(ScaleValue(24), Used + ScaleValue(6));
    Inc(Y, LineH);
  end;

  if PicW > 0 then
  begin
    PicH := ScaleValue(130);
    PicR := Rect(LW - PicW, BodyTop + ScaleValue(50), LW, BodyTop + ScaleValue(50) + PicH);
    APainter.RoundRect(PicR.Left, PicR.Top, PicR.Width, PicR.Height, ScaleValue(8),
      OBDMixColor(Palette.NeutralLight, Palette.GaugeFace, 0.35), Palette.NeutralLight);
    Graphic := nil;
    if (not UsePreviewData) and (Step < FSteps.Count) and (FSteps[Step].Picture.Graphic <> nil) and
      not FSteps[Step].Picture.Graphic.Empty then
      Graphic := FSteps[Step].Picture.Graphic;
    if Graphic <> nil then
      ACanvas.StretchDraw(Rect(PicR.Left + ScaleValue(8), PicR.Top + ScaleValue(8),
        PicR.Right - ScaleValue(8), PicR.Bottom - ScaleValue(28)), Graphic)
    else
      APainter.Glyph(VisualStepGlyph(Step), (PicR.Left + PicR.Right) div 2,
        (PicR.Top + PicR.Bottom) div 2 - ScaleValue(10), Palette.Subtle, 3);
    APainter.Text((PicR.Left + PicR.Right) div 2, PicR.Bottom - ScaleValue(16),
      VisualStepPictureCaption(Step), OBD_TEXT_SMALL, Palette.GaugeLabel,
      twRegular, taCenter, PicR.Width - ScaleValue(16));
  end;

  if VisualStepWarningTitle(Step) <> '' then
  begin
    BannerR := Rect(X, System.Math.Max(Y + ScaleValue(6), BodyTop + ScaleValue(194)),
      LW, System.Math.Max(Y + ScaleValue(58), BodyTop + ScaleValue(246)));
    if BannerR.Bottom < FootTop - ScaleValue(8) then
    begin
      APainter.BannerFrame(BannerR, bnWarning);
      APainter.BannerIcon(BannerR.Left + ScaleValue(24), (BannerR.Top + BannerR.Bottom) div 2, bnWarning, True);
      APainter.Text(BannerR.Left + ScaleValue(48), BannerR.Top + ScaleValue(18),
        VisualStepWarningTitle(Step), OBD_TEXT_BODY, Palette.ForegroundText, twSemibold,
        taLeftJustify, BannerR.Width - ScaleValue(64));
      APainter.Text(BannerR.Left + ScaleValue(48), BannerR.Top + ScaleValue(38),
        VisualStepWarningText(Step), OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular,
        taLeftJustify, BannerR.Width - ScaleValue(64));
    end;
  end;

  APainter.VLine(RX - ScaleValue(16), BodyTop - ScaleValue(4),
    FootTop - BodyTop - ScaleValue(8), Palette.NeutralLight);
  APainter.Caps(RX, BodyTop + ScaleValue(6), 'Live values');
  Y := BodyTop + ScaleValue(24);
  Save := SaveDC(ACanvas.Handle);
  try
    IntersectClipRect(ACanvas.Handle, RX, BodyTop, ClientWidth - ScaleValue(12), FootTop - ScaleValue(8));
    for I := 0 to VisualLiveCount - 1 do
    begin
      APainter.Text(RX, Y + ScaleValue(10), VisualLiveLabel(I), OBD_TEXT_BODY, Palette.GaugeLabel);
      ValueColor := StateColor(APainter, VisualLiveState(I));
      APainter.Text(RX + RightW, Y + ScaleValue(10), VisualLiveValue(I), OBD_TEXT_HEADING,
        ValueColor, twBold, taRightJustify, RightW);
      if VisualLiveShowBar(I) then
      begin
        Frac := EnsureRange(VisualLiveFraction(I), 0, 1);
        R := Rect(RX, Y + ScaleValue(24), RX + RightW, Y + ScaleValue(28));
        APainter.FillRect(R, Palette.NeutralLight);
        BarColor := ValueColor;
        if VisualLiveState(I) = opvNeutral then
          BarColor := Palette.Success;
        APainter.FillRect(Rect(R.Left, R.Top, R.Left + Round(R.Width * Frac), R.Bottom), BarColor);
      end;
      Inc(Y, ScaleValue(42));
    end;
  finally
    RestoreDC(ACanvas.Handle, Save);
  end;
end;

procedure TOBDProcedureView.PaintFooter(APainter: TOBDPainter);
var
  F: TRect;
  Source: string;
begin
  F := FooterRect;
  APainter.HLine(1, F.Top, ClientWidth - 2, Palette.NeutralLight);
  APainter.Button(ButtonRect(BTN_ABORT, APainter), 'Abort', bkDangerOutline, ButtonState(BTN_ABORT),
    Focused and (FHotButton = BTN_ABORT), glNone);
  Source := FSourceText;
  if UsePreviewData and (Source = '') then
    Source := 'Texts and pictures: catalogs/procedures/vag/epb-service.json';
  APainter.Text(ScaleValue(32) + APainter.ButtonWidth('Abort', glNone),
    F.Top + F.Height div 2, Source, OBD_TEXT_BODY, Palette.GaugeLabel,
    twRegular, taLeftJustify, ClientWidth - ScaleValue(460));
  APainter.Button(ButtonRect(BTN_BACK, APainter), 'Back', bkSecondary, ButtonState(BTN_BACK), False, glNone);
  if (StepCount > 0) and (VisualStepActionCaption(VisualCurrentStep) <> '') then
    APainter.Button(ButtonRect(BTN_ACTION, APainter), VisualStepActionCaption(VisualCurrentStep),
      bkSecondary, ButtonState(BTN_ACTION), False, glNone);
  APainter.Button(ButtonRect(BTN_NEXT, APainter), 'Next', bkPrimary, ButtonState(BTN_NEXT), False, glNone);
end;

procedure TOBDProcedureView.PaintEmpty(APainter: TOBDPainter);
var
  CX, CY: Integer;
begin
  PaintHeader(APainter);
  CX := ClientWidth div 2;
  CY := (ScaleValue(Metrics.Head) + FooterRect.Top) div 2;
  APainter.Glyph(glSteps, CX, CY - ScaleValue(26), Palette.Subtle, 1.8);
  APainter.Text(CX, CY + ScaleValue(4), 'No procedure steps', OBD_TEXT_HEADING,
    Palette.ForegroundText, twSemibold, taCenter);
  APainter.Text(CX, CY + ScaleValue(26), 'Load a catalogue procedure into the Steps collection.',
    OBD_TEXT_SMALL, Palette.GaugeLabel, twRegular, taCenter);
  PaintFooter(APainter);
end;

procedure TOBDProcedureView.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.Card(ClientRect, Palette.Accent);
    if StepCount = 0 then
      PaintEmpty(P)
    else
    begin
      PaintHeader(P);
      PaintStepStrip(P);
      PaintBody(P, ACanvas);
      PaintFooter(P);
    end;
  finally
    P.Free;
  end;
end;

procedure TOBDProcedureView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TOBDPainter;
  Step: Integer;
begin
  inherited;
  if (Button <> mbLeft) or not Enabled then
    Exit;
  if CanFocus and not Focused then
    SetFocus;
  Step := HitStep(X, Y);
  if Step >= 0 then
  begin
    CurrentStep := Step;
    Exit;
  end;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    FDownButton := HitButton(X, Y, P);
  finally
    P.Free;
  end;
  Invalidate;
end;

procedure TOBDProcedureView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TOBDPainter;
  NewHotButton, NewHotStep: Integer;
begin
  inherited;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    NewHotButton := HitButton(X, Y, P);
  finally
    P.Free;
  end;
  NewHotStep := HitStep(X, Y);
  if (FHotButton <> NewHotButton) or (FHotStep <> NewHotStep) then
  begin
    FHotButton := NewHotButton;
    FHotStep := NewHotStep;
    if (FHotButton <> BTN_NONE) or (FHotStep >= 0) then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
    Invalidate;
  end;
end;

procedure TOBDProcedureView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TOBDPainter;
  Hit, Down: Integer;
begin
  inherited;
  if Button <> mbLeft then
    Exit;
  Down := FDownButton;
  FDownButton := BTN_NONE;
  P := TOBDPainter.Create(Canvas, Palette, ScaleValue(96));
  try
    Hit := HitButton(X, Y, P);
  finally
    P.Free;
  end;
  if (Hit = Down) and (ButtonState(Hit) <> cstDisabled) then
    case Hit of
      BTN_ABORT:
        if Assigned(FOnAbort) then
          FOnAbort(Self);
      BTN_BACK:
        if Assigned(FOnBack) then
          FOnBack(Self)
        else
          CurrentStep := VisualCurrentStep - 1;
      BTN_ACTION:
        if Assigned(FOnAction) then
          FOnAction(Self);
      BTN_NEXT:
        if Assigned(FOnNext) then
          FOnNext(Self)
        else
          CurrentStep := VisualCurrentStep + 1;
    end;
  Invalidate;
end;

procedure TOBDProcedureView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT:
      CurrentStep := VisualCurrentStep - 1;
    VK_RIGHT:
      CurrentStep := VisualCurrentStep + 1;
    VK_HOME:
      CurrentStep := 0;
    VK_END:
      CurrentStep := StepCount - 1;
  else
    Exit;
  end;
  Key := 0;
end;

procedure TOBDProcedureView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHotButton := BTN_NONE;
  FHotStep := -1;
  Cursor := crDefault;
  Invalidate;
end;

procedure TOBDProcedureView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
