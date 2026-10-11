//------------------------------------------------------------------------------
//  ERD.UI.PrintDialogs
//
//  Themed print and page setup dialogs for OBD Studio.
//
//    TOBDPrintDialog       themed replacement for TPrintDialog: printer
//                          choice, pages, copies, orientation and first-page
//                          report preview.
//    TOBDPageSetupDialog   themed replacement for TPageSetupDialog and
//                          TPrinterSetupDialog: paper, units, orientation,
//                          margins and live page preview.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the themed print dialogs.
//------------------------------------------------------------------------------

unit ERD.UI.PrintDialogs;

interface

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Printers,
  ERD.UI.Types,
  ERD.UI.Control,
  ERD.UI.Paint,
  ERD.UI.Buttons,
  ERD.UI.Edits,
  ERD.UI.SpinEdit,
  ERD.UI.Segmented,
  ERD.UI.Chips,
  ERD.UI.Dialogs,
  ERD.UI.Backstage;

type
  /// <summary>Printed pages requested by <see cref="TOBDPrintDialog"/>.
  /// </summary>
  TOBDPrintRange = (
    /// <summary>Print every page.</summary>
    oprAllPages,
    /// <summary>Print the current selection.</summary>
    oprSelection,
    /// <summary>Print only the pages entered by the user.</summary>
    oprPageNums);

  /// <summary>Optional sections of <see cref="TOBDPrintDialog"/>.</summary>
  TOBDPrintOption = (
    /// <summary>Allow entering page numbers.</summary>
    opoPageNums,
    /// <summary>Allow printing the current selection.</summary>
    opoSelection,
    /// <summary>Show the Print to file check box.</summary>
    opoPrintToFile,
    /// <summary>Show Print to file but keep it disabled.</summary>
    opoDisablePrintToFile);

  /// <summary>Set of print-dialog options.</summary>
  TOBDPrintOptions = set of TOBDPrintOption;

  /// <summary>Units shown and returned by the page setup dialog.</summary>
  TOBDPageUnits = (
    /// <summary>Millimetres.</summary>
    opuMillimeters,
    /// <summary>Inches.</summary>
    opuInches);

  /// <summary>Themed replacement for <c>Vcl.Dialogs.TPrintDialog</c>.
  /// </summary>
  TOBDPrintDialog = class(TOBDCommonDialog)
  strict private
    FCopies: Integer;
    FCollate: Boolean;
    FFromPage: Integer;
    FToPage: Integer;
    FMinPage: Integer;
    FMaxPage: Integer;
    FPrintRange: TOBDPrintRange;
    FOptions: TOBDPrintOptions;
    FPrintToFile: Boolean;
    FPrinterIndex: Integer;
    FPrinterName: string;
    FOrientation: TPrinterOrientation;
    FOnPaintPage: TOBDReportPaintPageEvent;
    procedure SetCopies(AValue: Integer);
    procedure SetFromPage(AValue: Integer);
    procedure SetToPage(AValue: Integer);
    procedure SetMinPage(AValue: Integer);
    procedure SetMaxPage(AValue: Integer);
    procedure SetPrinterIndex(AValue: Integer);
    procedure SetPrinterName(const AValue: string);
  protected
    /// <summary>Title used while Title is empty.</summary>
    /// <returns>Default title.</returns>
    function DefaultTitle: string; override;
  public
    /// <summary>Creates the print dialog with VCL-compatible defaults.
    /// </summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Builds and shows the themed modal print dialog.</summary>
    /// <returns>True when the user clicked Print.</returns>
    function Execute: Boolean; override;
  published
    /// <summary>Number of copies to print.</summary>
    property Copies: Integer read FCopies write SetCopies default 1;
    /// <summary>Collate the printed copies.</summary>
    property Collate: Boolean read FCollate write FCollate default False;
    /// <summary>First page requested by the page range.</summary>
    property FromPage: Integer read FFromPage write SetFromPage default 1;
    /// <summary>Last page requested by the page range.</summary>
    property ToPage: Integer read FToPage write SetToPage default 1;
    /// <summary>Lowest page number the dialog accepts.</summary>
    property MinPage: Integer read FMinPage write SetMinPage default 1;
    /// <summary>Highest page number the dialog accepts.</summary>
    property MaxPage: Integer read FMaxPage write SetMaxPage default 9999;
    /// <summary>What part of the document to print.</summary>
    property PrintRange: TOBDPrintRange read FPrintRange write FPrintRange
      default oprAllPages;
    /// <summary>Sections and choices enabled in the dialog.</summary>
    property Options: TOBDPrintOptions read FOptions write FOptions;
    /// <summary>Whether Print to file is checked.</summary>
    property PrintToFile: Boolean read FPrintToFile write FPrintToFile
      default False;
    /// <summary>Index of the selected VCL printer, or -1.</summary>
    property PrinterIndex: Integer read FPrinterIndex write SetPrinterIndex
      default -1;
    /// <summary>Name of the selected VCL printer.</summary>
    property PrinterName: string read FPrinterName write SetPrinterName;
    /// <summary>Printer orientation applied when the dialog is accepted.
    /// </summary>
    property Orientation: TPrinterOrientation read FOrientation
      write FOrientation default poPortrait;
    /// <summary>Fires to paint the first report page in the preview.</summary>
    property OnPaintPage: TOBDReportPaintPageEvent read FOnPaintPage
      write FOnPaintPage;
  end;

  /// <summary>Themed replacement for <c>TPageSetupDialog</c> and
  /// <c>TPrinterSetupDialog</c>.</summary>
  TOBDPageSetupDialog = class(TOBDCommonDialog)
  strict private
    FPaperName: string;
    FPaperWidth: Double;
    FPaperHeight: Double;
    FOrientation: TPrinterOrientation;
    FMarginLeft: Double;
    FMarginTop: Double;
    FMarginRight: Double;
    FMarginBottom: Double;
    FUnits: TOBDPageUnits;
    procedure SetPaperName(const AValue: string);
    procedure SetPaperWidth(AValue: Double);
    procedure SetPaperHeight(AValue: Double);
    procedure SetMarginLeft(AValue: Double);
    procedure SetMarginTop(AValue: Double);
    procedure SetMarginRight(AValue: Double);
    procedure SetMarginBottom(AValue: Double);
    procedure SetUnits(AValue: TOBDPageUnits);
  protected
    /// <summary>Title used while Title is empty.</summary>
    /// <returns>Default title.</returns>
    function DefaultTitle: string; override;
  public
    /// <summary>Creates an A4 portrait page setup dialog.</summary>
    /// <param name="AOwner">Component owner.</param>
    constructor Create(AOwner: TComponent); override;
    /// <summary>Builds and shows the themed modal page setup dialog.</summary>
    /// <returns>True when the user clicked OK.</returns>
    function Execute: Boolean; override;
  published
    /// <summary>Name of the selected or custom paper.</summary>
    property PaperName: string read FPaperName write SetPaperName;
    /// <summary>Paper width in the selected units.</summary>
    property PaperWidth: Double read FPaperWidth write SetPaperWidth;
    /// <summary>Paper height in the selected units.</summary>
    property PaperHeight: Double read FPaperHeight write SetPaperHeight;
    /// <summary>Page orientation.</summary>
    property Orientation: TPrinterOrientation read FOrientation
      write FOrientation default poPortrait;
    /// <summary>Left margin in the selected units.</summary>
    property MarginLeft: Double read FMarginLeft write SetMarginLeft;
    /// <summary>Top margin in the selected units.</summary>
    property MarginTop: Double read FMarginTop write SetMarginTop;
    /// <summary>Right margin in the selected units.</summary>
    property MarginRight: Double read FMarginRight write SetMarginRight;
    /// <summary>Bottom margin in the selected units.</summary>
    property MarginBottom: Double read FMarginBottom write SetMarginBottom;
    /// <summary>Units used by paper size and margins.</summary>
    property Units: TOBDPageUnits read FUnits write SetUnits
      default opuMillimeters;
  end;

implementation

const
  DESIGN_PAPER_W = 420;
  PAPER_INK = TColor($001A1A1A);
  PAPER_MUTED = TColor($005C636A);
  PAPER_RULE = TColor($00E6E2E2);
  PAPER_READY_FILL = TColor($00D6F5FF);
  PAPER_WARN = TColor($00046485);
  PAPER_DANGER = TColor($004535DC);
  PAPER_ACCENT_TEXT = TColor($000A53B4);
  MM_PER_INCH = 25.4;

  PAPER_COUNT = 7;
  PAPER_NAMES: array[0..PAPER_COUNT - 1] of string = (
    'A4', 'A5', 'Letter', 'Legal', 'Executive', 'B5', 'Envelope DL');
  PAPER_WIDTHS_MM: array[0..PAPER_COUNT - 1] of Double = (
    210.0, 148.0, 215.9, 215.9, 184.2, 176.0, 110.0);
  PAPER_HEIGHTS_MM: array[0..PAPER_COUNT - 1] of Double = (
    297.0, 210.0, 279.4, 355.6, 266.7, 250.0, 220.0);

type
  TPrintPreviewPanel = class(TOBDCustomControl)
  strict private
    FEventSender: TObject;
    FPaperWidthMM: Double;
    FPaperHeightMM: Double;
    FOrientation: TPrinterOrientation;
    FOnPaintPage: TOBDReportPaintPageEvent;
    function PaperRect: TRect;
    function PageBackground: TColor;
    procedure SetOrientation(AValue: TPrinterOrientation);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor CreatePreview(AOwner: TComponent; AEventSender: TObject);
    procedure SetPaper(APaperWidthMM, APaperHeightMM: Double);
    property Orientation: TPrinterOrientation read FOrientation
      write SetOrientation;
    property OnPaintPage: TOBDReportPaintPageEvent read FOnPaintPage
      write FOnPaintPage;
  end;

  TPageSetupPreview = class(TOBDCustomControl)
  strict private
    FPaperWidthMM: Double;
    FPaperHeightMM: Double;
    FMarginLeftMM: Double;
    FMarginTopMM: Double;
    FMarginRightMM: Double;
    FMarginBottomMM: Double;
    FOrientation: TPrinterOrientation;
    function PaperRect: TRect;
    function PageBackground: TColor;
    procedure SetOrientation(AValue: TPrinterOrientation);
  protected
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetPaper(APaperWidthMM, APaperHeightMM: Double);
    procedure SetMargins(ALeftMM, ATopMM, ARightMM, ABottomMM: Double);
    property Orientation: TPrinterOrientation read FOrientation
      write SetOrientation;
  end;

  TPrintController = class(TComponent)
  strict private
    FDialog: TOBDPrintDialog;
    FPrinters: TStrings;
    FCombo: TOBDComboBox;
    FStatus: TOBDDialogText;
    FAllRadio: TOBDRadioButton;
    FPagesRadio: TOBDRadioButton;
    FSelectionRadio: TOBDRadioButton;
    FPagesEdit: TOBDEdit;
    FCopies: TOBDSpinEdit;
    FCollate: TOBDCheckBox;
    FOrientation: TOBDSegmented;
    FPrintToFile: TOBDCheckBox;
    FPreview: TPrintPreviewPanel;
    FPrintButton: TOBDButton;
    FNoPrinters: Boolean;
    procedure RangeChanged(Sender: TObject);
    procedure PrinterChanged(Sender: TObject);
    procedure OrientationChanged(Sender: TObject);
    function PrinterStatusText: string;
  public
    constructor CreateController(AOwner: TComponent;
      ADialog: TOBDPrintDialog; APrinters: TStrings; ANoPrinters: Boolean);
    procedure Bind(ACombo: TOBDComboBox; AStatus: TOBDDialogText;
      AAllRadio, APagesRadio, ASelectionRadio: TOBDRadioButton;
      APagesEdit: TOBDEdit; ACopies: TOBDSpinEdit; ACollate: TOBDCheckBox;
      AOrientation: TOBDSegmented; APrintToFile: TOBDCheckBox;
      APreview: TPrintPreviewPanel; APrintButton: TOBDButton);
    procedure ApplyToDialog;
  end;

  TPageSetupController = class(TComponent)
  strict private
    FDialog: TOBDPageSetupDialog;
    FPaperCombo: TOBDComboBox;
    FUnitsCombo: TOBDComboBox;
    FOrientation: TOBDSegmented;
    FLeft: TOBDSpinEdit;
    FTop: TOBDSpinEdit;
    FRight: TOBDSpinEdit;
    FBottom: TOBDSpinEdit;
    FMarginsCaption: TOBDDialogText;
    FPreview: TPageSetupPreview;
    FUnits: TOBDPageUnits;
    FUpdating: Boolean;
    procedure PaperChanged(Sender: TObject);
    procedure UnitsChanged(Sender: TObject);
    procedure OrientationChanged(Sender: TObject);
    procedure MarginsChanged(Sender: TObject);
    procedure FillPaperItems;
    procedure UpdateUnits;
    procedure UpdatePreview;
    function SelectedPaperIndex: Integer;
  public
    constructor CreateController(AOwner: TComponent;
      ADialog: TOBDPageSetupDialog);
    procedure Bind(APaperCombo, AUnitsCombo: TOBDComboBox;
      AOrientation: TOBDSegmented; ALeft, ATop, ARight,
      ABottom: TOBDSpinEdit; AMarginsCaption: TOBDDialogText;
      APreview: TPageSetupPreview);
    procedure ApplyToDialog;
  end;

function ClampInteger(AValue, AMin, AMax: Integer): Integer;
begin
  Result := AValue;
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

function Positive(AValue, ADefault: Double): Double;
begin
  if AValue > 0 then
    Result := AValue
  else
    Result := ADefault;
end;

function UnitsToMM(AValue: Double; AUnits: TOBDPageUnits): Double;
begin
  if AUnits = opuInches then
    Result := AValue * MM_PER_INCH
  else
    Result := AValue;
end;

function MMToUnits(AValue: Double; AUnits: TOBDPageUnits): Double;
begin
  if AUnits = opuInches then
    Result := AValue / MM_PER_INCH
  else
    Result := AValue;
end;

function UnitsName(AUnits: TOBDPageUnits): string;
begin
  if AUnits = opuInches then
    Result := 'in'
  else
    Result := 'mm';
end;

function FormatUnits(AValue: Double; AUnits: TOBDPageUnits): string;
begin
  if AUnits = opuInches then
    Result := FormatFloat('0.##', AValue)
  else
    Result := FormatFloat('0', AValue);
end;

function FormatPaperItem(AIndex: Integer; AUnits: TOBDPageUnits): string;
var
  W, H: Double;
begin
  W := MMToUnits(PAPER_WIDTHS_MM[AIndex], AUnits);
  H := MMToUnits(PAPER_HEIGHTS_MM[AIndex], AUnits);
  Result := Format('%s (%s x %s %s)', [PAPER_NAMES[AIndex],
    FormatUnits(W, AUnits), FormatUnits(H, AUnits), UnitsName(AUnits)]);
end;

function FindPaperIndex(const AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to PAPER_COUNT - 1 do
    if SameText(AName, PAPER_NAMES[I]) then
      Exit(I);
end;

function SafeCollectPrinters(AStrings: TStrings): Boolean;
var
  I: Integer;
begin
  Result := False;
  AStrings.Clear;
  try
    for I := 0 to Printer.Printers.Count - 1 do
      AStrings.Add(Printer.Printers[I]);
    Result := AStrings.Count = 0;
  except
    on E: EPrinter do
    begin
      AStrings.Clear;
      Result := True;
    end;
  end;
end;

function SafePrinterIndex: Integer;
begin
  Result := -1;
  try
    if Printer.Printers.Count > 0 then
      Result := Printer.PrinterIndex;
  except
    on E: EPrinter do
      Result := -1;
  end;
end;

procedure SafeApplyPrinter(AIndex: Integer; AOrientation: TPrinterOrientation);
begin
  try
    if (Printer.Printers.Count > 0) and (AIndex >= 0) and
      (AIndex < Printer.Printers.Count) then
    begin
      Printer.PrinterIndex := AIndex;
      Printer.Orientation := AOrientation;
    end;
  except
    on E: EPrinter do
      ;
  end;
end;

function ParsePageRange(const AText: string; AMin, AMax: Integer;
  out AFrom, ATo: Integer): Boolean;
var
  I, StartValue, EndValue, Code: Integer;
  S: string;
begin
  Result := False;
  AFrom := AMin;
  ATo := AMin;
  I := 1;
  while (I <= Length(AText)) and not CharInSet(AText[I], ['0'..'9']) do
    Inc(I);
  S := '';
  while (I <= Length(AText)) and CharInSet(AText[I], ['0'..'9']) do
  begin
    S := S + AText[I];
    Inc(I);
  end;
  if S = '' then
    Exit;
  Val(S, StartValue, Code);
  if Code <> 0 then
    Exit;
  EndValue := StartValue;
  while (I <= Length(AText)) and (AText[I] <> '-') do
    if CharInSet(AText[I], ['0'..'9', ',']) then
      Break
    else
      Inc(I);
  if (I <= Length(AText)) and (AText[I] = '-') then
  begin
    Inc(I);
    while (I <= Length(AText)) and not CharInSet(AText[I], ['0'..'9']) do
      Inc(I);
    S := '';
    while (I <= Length(AText)) and CharInSet(AText[I], ['0'..'9']) do
    begin
      S := S + AText[I];
      Inc(I);
    end;
    if S <> '' then
    begin
      Val(S, EndValue, Code);
      if Code <> 0 then
        EndValue := StartValue;
    end;
  end;
  AFrom := ClampInteger(StartValue, AMin, AMax);
  ATo := ClampInteger(EndValue, AMin, AMax);
  if ATo < AFrom then
    ATo := AFrom;
  Result := True;
end;

procedure DrawNeutralReport(APainter: TOBDPainter;
  const APalette: TOBDThemePalette; const R: TRect; AScale: Single);
var
  K: Single;
  L, RR, YY, I, RowH: Integer;
  Colr: TColor;
begin
  K := AScale;
  if K <= 0 then
    K := 1;
  L := R.Left + Round(28 * K);
  RR := R.Right - Round(28 * K);
  APainter.FillRect(Rect(L, R.Top + Round(28 * K), L + Round(34 * K),
    R.Top + Round(62 * K)), APalette.Accent);
  APainter.Text(L + Round(17 * K), R.Top + Round(45 * K), 'OBD',
    OBD_TEXT_SMALL * K, OBDContrastInk(APalette.Accent, PAPER_INK, clWhite),
    twBold, taCenter);
  APainter.Text(L + Round(44 * K), R.Top + Round(38 * K), 'Garage report',
    OBD_TEXT_BODY * K, PAPER_INK, twBold);
  APainter.Text(L + Round(44 * K), R.Top + Round(56 * K),
    'Diagnostic report - job 2026-0142', OBD_TEXT_SMALL * K, PAPER_MUTED);
  APainter.Text(RR, R.Top + Round(38 * K), '11 Oct 2026',
    OBD_TEXT_SMALL * K, PAPER_MUTED, twRegular, taRightJustify);
  APainter.Text(RR, R.Top + Round(56 * K), 'Page 1 of 3',
    OBD_TEXT_SMALL * K, PAPER_MUTED, twRegular, taRightJustify);
  APainter.FillRect(Rect(L, R.Top + Round(76 * K), RR,
    R.Top + Round(78 * K)), APalette.Accent);
  YY := R.Top + Round(104 * K);
  APainter.Text(L, YY, 'VEHICLE', OBD_TEXT_SMALL * K, PAPER_MUTED,
    twSemibold);
  Inc(YY, Round(20 * K));
  APainter.Text(L, YY, 'Volkswagen Golf VII 1.6 TDI',
    OBD_TEXT_BODY * K, PAPER_INK, twSemibold);
  APainter.Text(RR, YY, 'VIN WVWZZZAUZGW123456',
    OBD_TEXT_SMALL * K, PAPER_MUTED, twRegular, taRightJustify);
  Inc(YY, Round(30 * K));
  APainter.Text(L, YY, 'TROUBLE CODES', OBD_TEXT_SMALL * K, PAPER_MUTED,
    twSemibold);
  RowH := Round(24 * K);
  Inc(YY, Round(14 * K));
  for I := 0 to 3 do
  begin
    if I = 2 then
      Colr := PAPER_WARN
    else
      Colr := PAPER_DANGER;
    APainter.FillRect(Rect(L, YY + Round(4 * K), L + Round(3 * K),
      YY + RowH - Round(4 * K)), Colr);
    APainter.Text(L + Round(10 * K), YY + RowH div 2,
      'P0' + IntToStr(401 + I), OBD_TEXT_SMALL * K, PAPER_INK, twMonoBold);
    APainter.Text(L + Round(64 * K), YY + RowH div 2,
      'Diagnostic trouble code summary', OBD_TEXT_SMALL * K, PAPER_INK,
      twRegular, taLeftJustify, RR - L - Round(64 * K));
    Inc(YY, RowH);
    APainter.HLine(L, YY, RR - L, PAPER_RULE);
  end;
  Inc(YY, Round(26 * K));
  APainter.FillRect(Rect(L, YY, RR, YY + Round(38 * K)), PAPER_READY_FILL);
  APainter.FillRect(Rect(L, YY, L + Round(3 * K), YY + Round(38 * K)),
    PAPER_WARN);
  APainter.Text(L + Round(12 * K), YY + Round(13 * K),
    'Readiness not complete', OBD_TEXT_BODY * K, PAPER_INK, twBold);
  APainter.Text(L + Round(12 * K), YY + Round(29 * K),
    '2 of 8 supported monitors incomplete', OBD_TEXT_SMALL * K, PAPER_MUTED);
  Inc(YY, Round(64 * K));
  APainter.Text(L, YY, 'TECHNICIAN NOTES', OBD_TEXT_SMALL * K, PAPER_MUTED,
    twSemibold);
  for I := 0 to 3 do
    APainter.HLine(L, YY + Round((18 + I * 15) * K), RR - L, PAPER_RULE);
  APainter.HLine(L, R.Bottom - Round(34 * K), RR - L, PAPER_RULE);
  APainter.Text(L, R.Bottom - Round(22 * K),
    'Garage Peeters - Industrieweg 12 - Hasselt',
    OBD_TEXT_SMALL * K, PAPER_MUTED);
end;

{ TPrintPreviewPanel }

constructor TPrintPreviewPanel.CreatePreview(AOwner: TComponent;
  AEventSender: TObject);
begin
  inherited Create(AOwner);
  FEventSender := AEventSender;
  FPaperWidthMM := 210;
  FPaperHeightMM := 297;
  FOrientation := poPortrait;
  TabStop := False;
  Width := ScaleValue(260);
  Height := ScaleValue(360);
end;

function TPrintPreviewPanel.PageBackground: TColor;
begin
  Result := OBDMixColor(Palette.NeutralLight, Palette.Background, 0.5);
end;

procedure TPrintPreviewPanel.SetPaper(APaperWidthMM, APaperHeightMM: Double);
begin
  FPaperWidthMM := Positive(APaperWidthMM, 210);
  FPaperHeightMM := Positive(APaperHeightMM, 297);
  Invalidate;
end;

procedure TPrintPreviewPanel.SetOrientation(AValue: TPrinterOrientation);
begin
  if FOrientation = AValue then
    Exit;
  FOrientation := AValue;
  Invalidate;
end;

function TPrintPreviewPanel.PaperRect: TRect;
var
  PWMM, PHMM, Aspect: Double;
  AW, AH, PW, PH, X, Y: Integer;
begin
  PWMM := FPaperWidthMM;
  PHMM := FPaperHeightMM;
  if (FOrientation = poLandscape) and (PHMM > PWMM) then
  begin
    PWMM := FPaperHeightMM;
    PHMM := FPaperWidthMM;
  end;
  Aspect := PHMM / PWMM;
  AW := System.Math.Max(1, Width - ScaleValue(48));
  AH := System.Math.Max(1, Height - ScaleValue(62));
  PW := AW;
  PH := Round(PW * Aspect);
  if PH > AH then
  begin
    PH := AH;
    PW := Round(PH / Aspect);
  end;
  X := (Width - PW) div 2;
  Y := ScaleValue(18);
  Result := Rect(X, Y, X + PW, Y + PH);
end;

procedure TPrintPreviewPanel.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  R: TRect;
  Scale: Single;
  SaveIndex: Integer;
  S: string;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, PageBackground);
    P.FrameRect(ClientRect, Palette.NeutralLight);
    R := PaperRect;
    P.FillRect(Rect(R.Left + ScaleValue(5), R.Top + ScaleValue(5),
      R.Right + ScaleValue(5), R.Bottom + ScaleValue(5)),
      OBDMixColor(clBlack, PageBackground, 0.12));
    P.FillRect(R, clWhite);
    P.FrameRect(R, PAPER_RULE);
    Scale := R.Width / DESIGN_PAPER_W;
    SaveIndex := SaveDC(ACanvas.Handle);
    try
      IntersectClipRect(ACanvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      if Assigned(FOnPaintPage) and not IsPreview then
        FOnPaintPage(FEventSender, ACanvas, R, 0, Scale)
      else
        DrawNeutralReport(P, Palette, R, Scale);
    finally
      RestoreDC(ACanvas.Handle, SaveIndex);
    end;
    S := 'Page 1 of 3';
    P.Glyph(glChevronLeft, Width div 2 - P.TextWidth(S, OBD_TEXT_SMALL) div 2 -
      ScaleValue(18), Height - ScaleValue(22), Palette.Subtle, 0.8);
    P.Text(Width div 2, Height - ScaleValue(22), S, OBD_TEXT_SMALL,
      Palette.GaugeLabel, twRegular, taCenter);
    P.Glyph(glChevronRight, Width div 2 + P.TextWidth(S, OBD_TEXT_SMALL) div 2 +
      ScaleValue(18), Height - ScaleValue(22), Palette.Subtle, 0.8);
  finally
    P.Free;
  end;
end;

{ TPageSetupPreview }

constructor TPageSetupPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPaperWidthMM := 210;
  FPaperHeightMM := 297;
  FMarginLeftMM := 20;
  FMarginTopMM := 25;
  FMarginRightMM := 20;
  FMarginBottomMM := 20;
  FOrientation := poPortrait;
  TabStop := False;
  Width := ScaleValue(160);
  Height := ScaleValue(300);
end;

function TPageSetupPreview.PageBackground: TColor;
begin
  Result := OBDMixColor(Palette.NeutralLight, Palette.Background, 0.5);
end;

procedure TPageSetupPreview.SetPaper(APaperWidthMM, APaperHeightMM: Double);
begin
  FPaperWidthMM := Positive(APaperWidthMM, 210);
  FPaperHeightMM := Positive(APaperHeightMM, 297);
  Invalidate;
end;

procedure TPageSetupPreview.SetMargins(ALeftMM, ATopMM, ARightMM,
  ABottomMM: Double);
begin
  FMarginLeftMM := System.Math.Max(0.0, ALeftMM);
  FMarginTopMM := System.Math.Max(0.0, ATopMM);
  FMarginRightMM := System.Math.Max(0.0, ARightMM);
  FMarginBottomMM := System.Math.Max(0.0, ABottomMM);
  Invalidate;
end;

procedure TPageSetupPreview.SetOrientation(AValue: TPrinterOrientation);
begin
  if FOrientation = AValue then
    Exit;
  FOrientation := AValue;
  Invalidate;
end;

function TPageSetupPreview.PaperRect: TRect;
var
  PWMM, PHMM, Aspect: Double;
  AW, AH, PW, PH, X, Y: Integer;
begin
  PWMM := FPaperWidthMM;
  PHMM := FPaperHeightMM;
  if (FOrientation = poLandscape) and (PHMM > PWMM) then
  begin
    PWMM := FPaperHeightMM;
    PHMM := FPaperWidthMM;
  end;
  Aspect := PHMM / PWMM;
  AW := System.Math.Max(1, Width - ScaleValue(48));
  AH := System.Math.Max(1, Height - ScaleValue(36));
  PW := AW;
  PH := Round(PW * Aspect);
  if PH > AH then
  begin
    PH := AH;
    PW := Round(PH / Aspect);
  end;
  X := (Width - PW) div 2;
  Y := (Height - PH) div 2;
  Result := Rect(X, Y, X + PW, Y + PH);
end;

procedure TPageSetupPreview.PaintControl(ACanvas: TCanvas);
var
  P: TOBDPainter;
  R, M: TRect;
  PWMM, PHMM, KX, KY: Double;
  X, Y: Integer;
begin
  P := TOBDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    P.FillRect(ClientRect, PageBackground);
    P.FrameRect(ClientRect, Palette.NeutralLight);
    R := PaperRect;
    P.FillRect(Rect(R.Left + ScaleValue(5), R.Top + ScaleValue(5),
      R.Right + ScaleValue(5), R.Bottom + ScaleValue(5)),
      OBDMixColor(clBlack, PageBackground, 0.12));
    P.FillRect(R, clWhite);
    P.FrameRect(R, PAPER_RULE);
    PWMM := FPaperWidthMM;
    PHMM := FPaperHeightMM;
    if (FOrientation = poLandscape) and (PHMM > PWMM) then
    begin
      PWMM := FPaperHeightMM;
      PHMM := FPaperWidthMM;
    end;
    KX := R.Width / PWMM;
    KY := R.Height / PHMM;
    M := Rect(R.Left + Round(FMarginLeftMM * KX),
      R.Top + Round(FMarginTopMM * KY),
      R.Right - Round(FMarginRightMM * KX),
      R.Bottom - Round(FMarginBottomMM * KY));
    if M.Left >= M.Right then
      M.Right := M.Left + 1;
    if M.Top >= M.Bottom then
      M.Bottom := M.Top + 1;
    X := M.Left;
    while X < M.Right do
    begin
      P.HLine(X, M.Top, System.Math.Min(ScaleValue(4), M.Right - X),
        OBDMixColor(Palette.Accent, clWhite, 0.8));
      P.HLine(X, M.Bottom, System.Math.Min(ScaleValue(4), M.Right - X),
        OBDMixColor(Palette.Accent, clWhite, 0.8));
      Inc(X, ScaleValue(8));
    end;
    Y := M.Top;
    while Y < M.Bottom do
    begin
      P.VLine(M.Left, Y, System.Math.Min(ScaleValue(4), M.Bottom - Y),
        OBDMixColor(Palette.Accent, clWhite, 0.8));
      P.VLine(M.Right, Y, System.Math.Min(ScaleValue(4), M.Bottom - Y),
        OBDMixColor(Palette.Accent, clWhite, 0.8));
      Inc(Y, ScaleValue(8));
    end;
    P.FillRect(Rect(M.Left, M.Top + ScaleValue(8),
      M.Left + (M.Width div 3), M.Top + ScaleValue(13)), PAPER_MUTED);
    Y := M.Top + ScaleValue(26);
    while Y < M.Bottom - ScaleValue(8) do
    begin
      P.HLine(M.Left, Y, M.Width - ScaleValue(8), PAPER_RULE);
      Inc(Y, ScaleValue(14));
    end;
  finally
    P.Free;
  end;
end;

{ TPrintController }

constructor TPrintController.CreateController(AOwner: TComponent;
  ADialog: TOBDPrintDialog; APrinters: TStrings; ANoPrinters: Boolean);
begin
  inherited Create(AOwner);
  FDialog := ADialog;
  FPrinters := APrinters;
  FNoPrinters := ANoPrinters;
end;

procedure TPrintController.Bind(ACombo: TOBDComboBox;
  AStatus: TOBDDialogText; AAllRadio, APagesRadio,
  ASelectionRadio: TOBDRadioButton; APagesEdit: TOBDEdit;
  ACopies: TOBDSpinEdit; ACollate: TOBDCheckBox;
  AOrientation: TOBDSegmented; APrintToFile: TOBDCheckBox;
  APreview: TPrintPreviewPanel; APrintButton: TOBDButton);
var
  I, Selected: Integer;
begin
  FCombo := ACombo;
  FStatus := AStatus;
  FAllRadio := AAllRadio;
  FPagesRadio := APagesRadio;
  FSelectionRadio := ASelectionRadio;
  FPagesEdit := APagesEdit;
  FCopies := ACopies;
  FCollate := ACollate;
  FOrientation := AOrientation;
  FPrintToFile := APrintToFile;
  FPreview := APreview;
  FPrintButton := APrintButton;
  FCombo.Items.Assign(FPrinters);
  Selected := FDialog.PrinterIndex;
  if (Selected < 0) and (FDialog.PrinterName <> '') then
    Selected := FPrinters.IndexOf(FDialog.PrinterName);
  if (Selected < 0) and (FPrinters.Count > 0) then
    Selected := SafePrinterIndex;
  if (Selected < 0) and (FPrinters.Count > 0) then
    Selected := 0;
  if Selected >= FPrinters.Count then
    Selected := FPrinters.Count - 1;
  FCombo.ItemIndex := Selected;
  FCombo.Enabled := not FNoPrinters;
  FCombo.OnChange := PrinterChanged;
  FStatus.Caption := PrinterStatusText;
  FPrintButton.Enabled := not FNoPrinters;
  FAllRadio.OnChange := RangeChanged;
  FPagesRadio.OnChange := RangeChanged;
  FSelectionRadio.OnChange := RangeChanged;
  FPagesRadio.Enabled := opoPageNums in FDialog.Options;
  FPagesEdit.Enabled := FPagesRadio.Enabled;
  FSelectionRadio.Enabled := opoSelection in FDialog.Options;
  FPrintToFile.Visible := opoPrintToFile in FDialog.Options;
  FPrintToFile.Enabled := (opoPrintToFile in FDialog.Options) and
    not (opoDisablePrintToFile in FDialog.Options);
  case FDialog.PrintRange of
    oprSelection:
      FSelectionRadio.Checked := FSelectionRadio.Enabled;
    oprPageNums:
      FPagesRadio.Checked := FPagesRadio.Enabled;
  else
    FAllRadio.Checked := True;
  end;
  if not (FAllRadio.Checked or FPagesRadio.Checked or
    FSelectionRadio.Checked) then
    FAllRadio.Checked := True;
  FPagesEdit.Text := IntToStr(FDialog.FromPage);
  if FDialog.ToPage > FDialog.FromPage then
    FPagesEdit.Text := FPagesEdit.Text + '-' + IntToStr(FDialog.ToPage);
  FCopies.IntValue := FDialog.Copies;
  FCollate.Checked := FDialog.Collate;
  if FDialog.Orientation = poLandscape then
    I := 1
  else
    I := 0;
  FOrientation.ItemIndex := I;
  FOrientation.OnChange := OrientationChanged;
  FPrintToFile.Checked := FDialog.PrintToFile;
  FPreview.Orientation := FDialog.Orientation;
  FPreview.OnPaintPage := FDialog.OnPaintPage;
  RangeChanged(Self);
end;

function TPrintController.PrinterStatusText: string;
begin
  if FNoPrinters then
    Result := 'No printers are installed. Add a printer before printing.'
  else
    Result := 'Ready - A4 - duplex';
end;

procedure TPrintController.RangeChanged(Sender: TObject);
begin
  FPagesEdit.Enabled := FPagesRadio.Checked and FPagesRadio.Enabled;
end;

procedure TPrintController.PrinterChanged(Sender: TObject);
begin
  FStatus.Caption := PrinterStatusText;
end;

procedure TPrintController.OrientationChanged(Sender: TObject);
begin
  if FOrientation.ItemIndex = 1 then
    FPreview.Orientation := poLandscape
  else
    FPreview.Orientation := poPortrait;
end;

procedure TPrintController.ApplyToDialog;
var
  FromPage, ToPage: Integer;
begin
  FDialog.Copies := FCopies.IntValue;
  FDialog.Collate := FCollate.Checked;
  FDialog.PrintToFile := FPrintToFile.Visible and FPrintToFile.Checked;
  FDialog.PrinterIndex := FCombo.ItemIndex;
  if (FCombo.ItemIndex >= 0) and (FCombo.ItemIndex < FPrinters.Count) then
    FDialog.PrinterName := FPrinters[FCombo.ItemIndex]
  else
    FDialog.PrinterName := '';
  if FOrientation.ItemIndex = 1 then
    FDialog.Orientation := poLandscape
  else
    FDialog.Orientation := poPortrait;
  if FPagesRadio.Checked then
  begin
    FDialog.PrintRange := oprPageNums;
    if ParsePageRange(FPagesEdit.Text, FDialog.MinPage, FDialog.MaxPage,
      FromPage, ToPage) then
    begin
      FDialog.FromPage := FromPage;
      FDialog.ToPage := ToPage;
    end;
  end
  else if FSelectionRadio.Checked then
    FDialog.PrintRange := oprSelection
  else
    FDialog.PrintRange := oprAllPages;
  if FDialog.PrinterIndex >= 0 then
    SafeApplyPrinter(FDialog.PrinterIndex, FDialog.Orientation);
end;

{ TPageSetupController }

constructor TPageSetupController.CreateController(AOwner: TComponent;
  ADialog: TOBDPageSetupDialog);
begin
  inherited Create(AOwner);
  FDialog := ADialog;
  FUnits := ADialog.Units;
end;

procedure TPageSetupController.Bind(APaperCombo, AUnitsCombo: TOBDComboBox;
  AOrientation: TOBDSegmented; ALeft, ATop, ARight,
  ABottom: TOBDSpinEdit; AMarginsCaption: TOBDDialogText;
  APreview: TPageSetupPreview);
var
  Index: Integer;
begin
  FPaperCombo := APaperCombo;
  FUnitsCombo := AUnitsCombo;
  FOrientation := AOrientation;
  FLeft := ALeft;
  FTop := ATop;
  FRight := ARight;
  FBottom := ABottom;
  FMarginsCaption := AMarginsCaption;
  FPreview := APreview;
  FillPaperItems;
  Index := FindPaperIndex(FDialog.PaperName);
  if Index < 0 then
    Index := 0;
  FPaperCombo.ItemIndex := Index;
  FPaperCombo.OnChange := PaperChanged;
  FUnitsCombo.Items.Add('Millimeters');
  FUnitsCombo.Items.Add('Inches');
  FUnitsCombo.ItemIndex := Ord(FUnits);
  FUnitsCombo.OnChange := UnitsChanged;
  if FDialog.Orientation = poLandscape then
    FOrientation.ItemIndex := 1
  else
    FOrientation.ItemIndex := 0;
  FOrientation.OnChange := OrientationChanged;
  FLeft.Value := FDialog.MarginLeft;
  FTop.Value := FDialog.MarginTop;
  FRight.Value := FDialog.MarginRight;
  FBottom.Value := FDialog.MarginBottom;
  FLeft.OnValueChange := MarginsChanged;
  FTop.OnValueChange := MarginsChanged;
  FRight.OnValueChange := MarginsChanged;
  FBottom.OnValueChange := MarginsChanged;
  UpdateUnits;
  UpdatePreview;
end;

procedure TPageSetupController.FillPaperItems;
var
  I, Save: Integer;
begin
  Save := FPaperCombo.ItemIndex;
  FPaperCombo.Items.BeginUpdate;
  try
    FPaperCombo.Items.Clear;
    for I := 0 to PAPER_COUNT - 1 do
      FPaperCombo.Items.Add(FormatPaperItem(I, FUnits));
  finally
    FPaperCombo.Items.EndUpdate;
  end;
  if Save >= FPaperCombo.Items.Count then
    Save := FPaperCombo.Items.Count - 1;
  if Save >= 0 then
    FPaperCombo.ItemIndex := Save;
end;

procedure TPageSetupController.UpdateUnits;
var
  Decimals: Integer;
begin
  if FUnits = opuInches then
    Decimals := 2
  else
    Decimals := 0;
  FLeft.Decimals := Decimals;
  FTop.Decimals := Decimals;
  FRight.Decimals := Decimals;
  FBottom.Decimals := Decimals;
  if FUnits = opuInches then
    FLeft.Increment := 0.1
  else
    FLeft.Increment := 1.0;
  FTop.Increment := FLeft.Increment;
  FRight.Increment := FLeft.Increment;
  FBottom.Increment := FLeft.Increment;
  FMarginsCaption.Caption := 'Margins (' + UnitsName(FUnits) + ')';
end;

function TPageSetupController.SelectedPaperIndex: Integer;
begin
  Result := FPaperCombo.ItemIndex;
  if (Result < 0) or (Result >= PAPER_COUNT) then
    Result := 0;
end;

procedure TPageSetupController.PaperChanged(Sender: TObject);
begin
  if FUpdating then
    Exit;
  UpdatePreview;
end;

procedure TPageSetupController.UnitsChanged(Sender: TObject);
var
  OldUnits, NewUnits: TOBDPageUnits;
  OldLeft, OldTop, OldRight, OldBottom: Double;
  Index: Integer;
begin
  if FUpdating then
    Exit;
  OldUnits := FUnits;
  if FUnitsCombo.ItemIndex = 1 then
    NewUnits := opuInches
  else
    NewUnits := opuMillimeters;
  if OldUnits = NewUnits then
    Exit;
  OldLeft := FLeft.Value;
  OldTop := FTop.Value;
  OldRight := FRight.Value;
  OldBottom := FBottom.Value;
  FUpdating := True;
  try
    FUnits := NewUnits;
    UpdateUnits;
    FLeft.Value := MMToUnits(UnitsToMM(OldLeft, OldUnits), NewUnits);
    FTop.Value := MMToUnits(UnitsToMM(OldTop, OldUnits), NewUnits);
    FRight.Value := MMToUnits(UnitsToMM(OldRight, OldUnits), NewUnits);
    FBottom.Value := MMToUnits(UnitsToMM(OldBottom, OldUnits), NewUnits);
    Index := SelectedPaperIndex;
    FillPaperItems;
    FPaperCombo.ItemIndex := Index;
  finally
    FUpdating := False;
  end;
  UpdatePreview;
end;

procedure TPageSetupController.OrientationChanged(Sender: TObject);
begin
  if FOrientation.ItemIndex = 1 then
    FPreview.Orientation := poLandscape
  else
    FPreview.Orientation := poPortrait;
end;

procedure TPageSetupController.MarginsChanged(Sender: TObject);
begin
  if not FUpdating then
    UpdatePreview;
end;

procedure TPageSetupController.UpdatePreview;
var
  Index: Integer;
begin
  Index := SelectedPaperIndex;
  FPreview.SetPaper(PAPER_WIDTHS_MM[Index], PAPER_HEIGHTS_MM[Index]);
  if FOrientation.ItemIndex = 1 then
    FPreview.Orientation := poLandscape
  else
    FPreview.Orientation := poPortrait;
  FPreview.SetMargins(UnitsToMM(FLeft.Value, FUnits),
    UnitsToMM(FTop.Value, FUnits), UnitsToMM(FRight.Value, FUnits),
    UnitsToMM(FBottom.Value, FUnits));
end;

procedure TPageSetupController.ApplyToDialog;
var
  Index: Integer;
begin
  Index := SelectedPaperIndex;
  FDialog.Units := FUnits;
  FDialog.PaperName := PAPER_NAMES[Index];
  FDialog.PaperWidth := MMToUnits(PAPER_WIDTHS_MM[Index], FUnits);
  FDialog.PaperHeight := MMToUnits(PAPER_HEIGHTS_MM[Index], FUnits);
  if FOrientation.ItemIndex = 1 then
    FDialog.Orientation := poLandscape
  else
    FDialog.Orientation := poPortrait;
  FDialog.MarginLeft := FLeft.Value;
  FDialog.MarginTop := FTop.Value;
  FDialog.MarginRight := FRight.Value;
  FDialog.MarginBottom := FBottom.Value;
end;

{ TOBDPrintDialog }

constructor TOBDPrintDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCopies := 1;
  FFromPage := 1;
  FToPage := 1;
  FMinPage := 1;
  FMaxPage := 9999;
  FPrintRange := oprAllPages;
  FOptions := [opoPageNums, opoSelection, opoPrintToFile];
  FPrinterIndex := -1;
  FOrientation := poPortrait;
end;

function TOBDPrintDialog.DefaultTitle: string;
begin
  Result := 'Print';
end;

procedure TOBDPrintDialog.SetCopies(AValue: Integer);
begin
  FCopies := System.Math.Max(1, AValue);
end;

procedure TOBDPrintDialog.SetFromPage(AValue: Integer);
begin
  FFromPage := ClampInteger(AValue, FMinPage, FMaxPage);
  if FToPage < FFromPage then
    FToPage := FFromPage;
end;

procedure TOBDPrintDialog.SetToPage(AValue: Integer);
begin
  FToPage := ClampInteger(AValue, FMinPage, FMaxPage);
  if FFromPage > FToPage then
    FFromPage := FToPage;
end;

procedure TOBDPrintDialog.SetMinPage(AValue: Integer);
begin
  FMinPage := System.Math.Max(0, AValue);
  if FMaxPage < FMinPage then
    FMaxPage := FMinPage;
  SetFromPage(FFromPage);
  SetToPage(FToPage);
end;

procedure TOBDPrintDialog.SetMaxPage(AValue: Integer);
begin
  FMaxPage := System.Math.Max(FMinPage, AValue);
  SetFromPage(FFromPage);
  SetToPage(FToPage);
end;

procedure TOBDPrintDialog.SetPrinterIndex(AValue: Integer);
begin
  FPrinterIndex := AValue;
end;

procedure TOBDPrintDialog.SetPrinterName(const AValue: string);
begin
  FPrinterName := AValue;
end;

function TOBDPrintDialog.Execute: Boolean;
var
  Window: TOBDDialogWindow;
  Printers: TStringList;
  Controller: TPrintController;
  Combo: TOBDComboBox;
  Status: TOBDDialogText;
  AllRadio, PagesRadio, SelectionRadio: TOBDRadioButton;
  PagesEdit: TOBDEdit;
  CopiesEdit: TOBDSpinEdit;
  CollateBox, PrintFileBox: TOBDCheckBox;
  OrientationSeg: TOBDSegmented;
  Preview: TPrintPreviewPanel;
  PrintButton: TOBDButton;
  SetupButton: TOBDButton;
  Banner: TOBDBanner;
  NoPrinters: Boolean;
  BodyH, Offset, Y: Integer;
begin
  Printers := TStringList.Create;
  try
    NoPrinters := SafeCollectPrinters(Printers);
    if NoPrinters then
      BodyH := 462
    else
      BodyH := 404;
    Offset := 0;
    if NoPrinters then
      Offset := 66;
    Window := NewWindow;
    try
      Window.SetLayout(600, BodyH, True);
      Controller := TPrintController.CreateController(Window, Self, Printers,
        NoPrinters);
      Window.AddText(16, 16, 300, 'Printer');
      Combo := TOBDComboBox.Create(Window);
      Window.Adopt(Combo);
      Window.Place(Combo, 16, 34, 300, DensityMetrics(Density).Edit);
      Status := Window.AddText(32, 34 + DensityMetrics(Density).Edit + 11,
        284, '', dtsSmall);
      if NoPrinters then
      begin
        Banner := TOBDBanner.Create(Window);
        Window.Adopt(Banner);
        Banner.Kind := bnInfo;
        Banner.Title := 'No printers';
        Banner.Text := 'Add a printer in Windows before printing.';
        Banner.AlertIcon := False;
        Window.Place(Banner, 16, 80, 300, 56);
      end;
      Y := 80 + Offset;
      Window.AddText(16, Y, 300, 'Pages');
      Inc(Y, 18);
      AllRadio := TOBDRadioButton.Create(Window);
      Window.Adopt(AllRadio);
      AllRadio.Caption := 'All pages';
      AllRadio.GroupIndex := 1;
      Window.Place(AllRadio, 16, Y, 150, DensityMetrics(Density).Check + 12);
      Inc(Y, System.Math.Max(26, DensityMetrics(Density).Check + 12));
      PagesRadio := TOBDRadioButton.Create(Window);
      Window.Adopt(PagesRadio);
      PagesRadio.Caption := 'Pages';
      PagesRadio.GroupIndex := 1;
      Window.Place(PagesRadio, 16, Y, 84, DensityMetrics(Density).Edit + 4);
      PagesEdit := TOBDEdit.Create(Window);
      Window.Adopt(PagesEdit);
      Window.Place(PagesEdit, 104, Y + 2, 110, DensityMetrics(Density).Edit);
      Inc(Y, System.Math.Max(DensityMetrics(Density).Edit + 4,
        DensityMetrics(Density).Check + 12));
      SelectionRadio := TOBDRadioButton.Create(Window);
      Window.Adopt(SelectionRadio);
      SelectionRadio.Caption := 'Selection';
      SelectionRadio.GroupIndex := 1;
      Window.Place(SelectionRadio, 16, Y, 150,
        DensityMetrics(Density).Check + 12);
      Inc(Y, System.Math.Max(26, DensityMetrics(Density).Check + 12) + 8);
      Window.AddText(16, Y, 300, 'Copies');
      CopiesEdit := TOBDSpinEdit.Create(Window);
      Window.Adopt(CopiesEdit);
      CopiesEdit.MinValue := 1;
      CopiesEdit.MaxValue := 999;
      Window.Place(CopiesEdit, 16, Y + 18, 96, DensityMetrics(Density).Edit);
      CollateBox := TOBDCheckBox.Create(Window);
      Window.Adopt(CollateBox);
      CollateBox.Caption := 'Collate';
      Window.Place(CollateBox, 128, Y + 18, 130,
        DensityMetrics(Density).Edit);
      Inc(Y, 18 + DensityMetrics(Density).Edit + 14);
      Window.AddText(16, Y, 300, 'Orientation');
      OrientationSeg := TOBDSegmented.Create(Window);
      Window.Adopt(OrientationSeg);
      OrientationSeg.Items.Add('Portrait');
      OrientationSeg.Items.Add('Landscape');
      Window.Place(OrientationSeg, 16, Y + 18, 190,
        DensityMetrics(Density).Segment + 8);
      Inc(Y, 18 + DensityMetrics(Density).Segment + 14);
      PrintFileBox := TOBDCheckBox.Create(Window);
      Window.Adopt(PrintFileBox);
      PrintFileBox.Caption := 'Print to file';
      Window.Place(PrintFileBox, 16, Y, 180,
        DensityMetrics(Density).Check + 12);
      Preview := TPrintPreviewPanel.CreatePreview(Window, Self);
      Window.Adopt(Preview);
      Window.Place(Preview, 340, 16, 244, BodyH);
      PrintButton := Window.AddButton('Print', bkPrimary, mrOk);
      PrintButton.Default := True;
      Window.AddButton('Cancel', bkSecondary, mrCancel).Cancel := True;
      SetupButton := Window.AddLeftButton('Page setup', glGear);
      SetupButton.Enabled := True;
      Window.InitialFocus := Combo;
      Controller.Bind(Combo, Status, AllRadio, PagesRadio, SelectionRadio,
        PagesEdit, CopiesEdit, CollateBox, OrientationSeg, PrintFileBox,
        Preview, PrintButton);
      Result := RunWindow(Window) = mrOk;
      if Result then
        Controller.ApplyToDialog;
    finally
      Window.Free;
    end;
  finally
    Printers.Free;
  end;
end;

{ TOBDPageSetupDialog }

constructor TOBDPageSetupDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPaperName := 'A4';
  FPaperWidth := 210;
  FPaperHeight := 297;
  FOrientation := poPortrait;
  FMarginLeft := 20;
  FMarginTop := 25;
  FMarginRight := 20;
  FMarginBottom := 20;
  FUnits := opuMillimeters;
end;

function TOBDPageSetupDialog.DefaultTitle: string;
begin
  Result := 'Page setup';
end;

procedure TOBDPageSetupDialog.SetPaperName(const AValue: string);
var
  Index: Integer;
begin
  FPaperName := AValue;
  Index := FindPaperIndex(AValue);
  if Index >= 0 then
  begin
    FPaperWidth := MMToUnits(PAPER_WIDTHS_MM[Index], FUnits);
    FPaperHeight := MMToUnits(PAPER_HEIGHTS_MM[Index], FUnits);
  end;
end;

procedure TOBDPageSetupDialog.SetPaperWidth(AValue: Double);
begin
  FPaperWidth := Positive(AValue, FPaperWidth);
end;

procedure TOBDPageSetupDialog.SetPaperHeight(AValue: Double);
begin
  FPaperHeight := Positive(AValue, FPaperHeight);
end;

procedure TOBDPageSetupDialog.SetMarginLeft(AValue: Double);
begin
  FMarginLeft := System.Math.Max(0.0, AValue);
end;

procedure TOBDPageSetupDialog.SetMarginTop(AValue: Double);
begin
  FMarginTop := System.Math.Max(0.0, AValue);
end;

procedure TOBDPageSetupDialog.SetMarginRight(AValue: Double);
begin
  FMarginRight := System.Math.Max(0.0, AValue);
end;

procedure TOBDPageSetupDialog.SetMarginBottom(AValue: Double);
begin
  FMarginBottom := System.Math.Max(0.0, AValue);
end;

procedure TOBDPageSetupDialog.SetUnits(AValue: TOBDPageUnits);
var
  OldUnits: TOBDPageUnits;
begin
  if FUnits = AValue then
    Exit;
  OldUnits := FUnits;
  FPaperWidth := MMToUnits(UnitsToMM(FPaperWidth, OldUnits), AValue);
  FPaperHeight := MMToUnits(UnitsToMM(FPaperHeight, OldUnits), AValue);
  FMarginLeft := MMToUnits(UnitsToMM(FMarginLeft, OldUnits), AValue);
  FMarginTop := MMToUnits(UnitsToMM(FMarginTop, OldUnits), AValue);
  FMarginRight := MMToUnits(UnitsToMM(FMarginRight, OldUnits), AValue);
  FMarginBottom := MMToUnits(UnitsToMM(FMarginBottom, OldUnits), AValue);
  FUnits := AValue;
end;

function TOBDPageSetupDialog.Execute: Boolean;
var
  Window: TOBDDialogWindow;
  Controller: TPageSetupController;
  PaperCombo, SourceCombo, UnitsCombo: TOBDComboBox;
  OrientationSeg: TOBDSegmented;
  LeftSpin, TopSpin, RightSpin, BottomSpin: TOBDSpinEdit;
  MarginsCaption: TOBDDialogText;
  Preview: TPageSetupPreview;
  PrinterButton: TOBDButton;
  OKButton: TOBDButton;
  Y: Integer;
begin
  Window := NewWindow;
  try
    Window.SetLayout(436, 350, True);
    Controller := TPageSetupController.CreateController(Window, Self);
    Y := 16;
    Window.AddText(16, Y, 230, 'Paper size');
    PaperCombo := TOBDComboBox.Create(Window);
    Window.Adopt(PaperCombo);
    Window.Place(PaperCombo, 16, Y + 18, 230, DensityMetrics(Density).Edit);
    Inc(Y, 18 + DensityMetrics(Density).Edit + 12);
    Window.AddText(16, Y, 230, 'Source');
    SourceCombo := TOBDComboBox.Create(Window);
    Window.Adopt(SourceCombo);
    SourceCombo.Items.Add('Automatically select');
    SourceCombo.ItemIndex := 0;
    Window.Place(SourceCombo, 16, Y + 18, 230, DensityMetrics(Density).Edit);
    Inc(Y, 18 + DensityMetrics(Density).Edit + 12);
    Window.AddText(16, Y, 230, 'Units');
    UnitsCombo := TOBDComboBox.Create(Window);
    Window.Adopt(UnitsCombo);
    Window.Place(UnitsCombo, 16, Y + 18, 150, DensityMetrics(Density).Edit);
    Inc(Y, 18 + DensityMetrics(Density).Edit + 12);
    Window.AddText(16, Y, 230, 'Orientation');
    OrientationSeg := TOBDSegmented.Create(Window);
    Window.Adopt(OrientationSeg);
    OrientationSeg.Items.Add('Portrait');
    OrientationSeg.Items.Add('Landscape');
    Window.Place(OrientationSeg, 16, Y + 18, 190,
      DensityMetrics(Density).Segment + 8);
    Inc(Y, 18 + DensityMetrics(Density).Segment + 12);
    MarginsCaption := Window.AddText(16, Y, 230, 'Margins (mm)');
    Inc(Y, 18);
    Window.AddText(16, Y + 6, 42, 'Left', dtsSmall);
    LeftSpin := TOBDSpinEdit.Create(Window);
    Window.Adopt(LeftSpin);
    LeftSpin.MinValue := 0;
    Window.Place(LeftSpin, 60, Y, 60, DensityMetrics(Density).Edit);
    Window.AddText(134, Y + 6, 52, 'Right', dtsSmall);
    RightSpin := TOBDSpinEdit.Create(Window);
    Window.Adopt(RightSpin);
    RightSpin.MinValue := 0;
    Window.Place(RightSpin, 186, Y, 60, DensityMetrics(Density).Edit);
    Inc(Y, DensityMetrics(Density).Edit + 8);
    Window.AddText(16, Y + 6, 42, 'Top', dtsSmall);
    TopSpin := TOBDSpinEdit.Create(Window);
    Window.Adopt(TopSpin);
    TopSpin.MinValue := 0;
    Window.Place(TopSpin, 60, Y, 60, DensityMetrics(Density).Edit);
    Window.AddText(134, Y + 6, 52, 'Bottom', dtsSmall);
    BottomSpin := TOBDSpinEdit.Create(Window);
    Window.Adopt(BottomSpin);
    BottomSpin.MinValue := 0;
    Window.Place(BottomSpin, 186, Y, 60, DensityMetrics(Density).Edit);
    Preview := TPageSetupPreview.Create(Window);
    Window.Adopt(Preview);
    Window.Place(Preview, 270, 16, 150, 318);
    OKButton := Window.AddButton('OK', bkPrimary, mrOk);
    OKButton.Default := True;
    Window.AddButton('Cancel', bkSecondary, mrCancel).Cancel := True;
    PrinterButton := Window.AddLeftButton('Printer', glPrint);
    PrinterButton.Enabled := True;
    Window.InitialFocus := PaperCombo;
    Controller.Bind(PaperCombo, UnitsCombo, OrientationSeg, LeftSpin, TopSpin,
      RightSpin, BottomSpin, MarginsCaption, Preview);
    Result := RunWindow(Window) = mrOk;
    if Result then
      Controller.ApplyToDialog;
  finally
    Window.Free;
  end;
end;

end.
