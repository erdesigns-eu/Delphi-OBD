unit ERD.Components.LinkLabel;
interface
uses
  System.Classes, System.SysUtils, System.Types, Winapi.Windows, Winapi.ShellAPI,
  Vcl.Controls, Vcl.Graphics,
  ERD.Components.Types, ERD.Components.Control, ERD.Components.Paint,
  ERD.Components.Buttons, ERD.Components.Icons;
type
  TERDLinkLabel = class(TERDInteractiveControl)
  private
    FURL: string;
    FAutoOpen, FVisited, FAlwaysUnderline: Boolean;
    FIcon: TERDIcon;
    procedure SetURL(const Value: string);
    procedure SetVisited(Value: Boolean);
    procedure SetAlwaysUnderline(Value: Boolean);
    procedure SetIcon(Value: TERDIcon);
  protected
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure PaintControl(ACanvas: TCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>Explicitly opens URL using the Windows shell; False on failure.</summary>
    function OpenLink: Boolean;
  published
    property Caption;
    property URL: string read FURL write SetURL;
    /// <summary>False by default: OnClick lets the application handle navigation.</summary>
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen default False;
    property Visited: Boolean read FVisited write SetVisited default False;
    property AlwaysUnderline: Boolean read FAlwaysUnderline write SetAlwaysUnderline default False;
    property Icon: TERDIcon read FIcon write SetIcon;
    property OnClick;
  end;
implementation
constructor TERDLinkLabel.Create(AOwner: TComponent);
begin
  inherited;
  FIcon := TERDIcon.Create(Self);
  Cursor := crHandPoint;
  Caption := 'Link';
  Width := 80;
  Height := 28;
end;
destructor TERDLinkLabel.Destroy;
begin
  FIcon.Free;
  inherited;
end;
procedure TERDLinkLabel.SetURL(const Value: string);
begin
  if FURL = Value then Exit;
  FURL := Value;
  Visited := False;
end;
procedure TERDLinkLabel.SetVisited(Value: Boolean);
begin
  if FVisited = Value then Exit;
  FVisited := Value;
  Invalidate;
end;
procedure TERDLinkLabel.SetAlwaysUnderline(Value: Boolean);
begin
  if FAlwaysUnderline = Value then Exit;
  FAlwaysUnderline := Value;
  Invalidate;
end;
procedure TERDLinkLabel.SetIcon(Value: TERDIcon);
begin FIcon.Assign(Value); if AutoSize then AdjustSize; Invalidate end;
function TERDLinkLabel.OpenLink: Boolean;
begin
  Result := Enabled and (FURL <> '');
  if Result then Result := NativeInt(ShellExecute(0, 'open', PChar(FURL), nil, nil, SW_SHOWNORMAL)) > 32;
  if Result then Visited := True;
end;
procedure TERDLinkLabel.Click;
begin
  if not Enabled then Exit;
  Visited := True;
  if FAutoOpen then OpenLink;
  inherited;
end;
procedure TERDLinkLabel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = []) and Enabled then
  begin Key := 0; Click end else inherited;
end;
function TERDLinkLabel.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  NewWidth := MeasureText(Caption, ERD_TEXT_BODY, twRegular) + 2 * RingMargin + ScaleValue(4);
  if (FIcon <> nil) and FIcon.Available then Inc(NewWidth, ScaleValue(24));
  NewHeight := ScaleValue(24) + 2 * RingMargin;
end;
procedure TERDLinkLabel.PaintControl(ACanvas: TCanvas);
var P: TERDPainter; Ink: TColor; X, DrawnWidth, CY: Integer;
begin
  P := TERDPainter.Create(ACanvas, Palette, ScaleValue(96));
  try
    Ink := Palette.Accent;
    if FVisited then Ink := ERDMixColor(Ink, Palette.ForegroundText, 0.35);
    if not Enabled then Ink := P.DisabledText
    else if InteractionState = cstPressed then Ink := ERDMixColor(Ink, Palette.ForegroundText, 0.25);
    X := RingMargin + P.S(2);
    CY := Height div 2;
    if (FIcon <> nil) and FIcon.Available then
    begin FIcon.Draw(ACanvas, Palette, ScaleValue(96), X + P.S(8), CY, Ink, 1, Enabled); Inc(X, P.S(24)) end;
    DrawnWidth := P.Text(X, CY, Caption, ERD_TEXT_BODY, Ink, twRegular,
      taLeftJustify, Width - X - RingMargin);
    if FAlwaysUnderline or Hover or HasFocus then P.HLine(X, CY + P.S(8), DrawnWidth, Ink);
    if HasFocus then P.FocusRing(RingMargin, RingMargin, Width - 2 * RingMargin, Height - 2 * RingMargin, P.S(2));
  finally P.Free end;
end;
end.
