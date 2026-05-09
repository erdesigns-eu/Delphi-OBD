//------------------------------------------------------------------------------
// UNIT           : OBD.Render.DtcList.pas
// CONTENTS       : Framework-neutral renderer for the virtualised DTC list
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
// NOTE           : Pure-Skia renderer. Item shape is duplicated as a
//                  flat record so this unit can be referenced by both
//                  the VCL `OBD.DtcList` and the FMX `OBD.DtcList.FMX`
//                  without dragging either's full collection class
//                  through the renderer.
//------------------------------------------------------------------------------
unit OBD.Render.DtcList;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Math, System.Skia;

type
  TOBDDtcSeverity = (dsInfo, dsWarning, dsCritical);
  TOBDDtcStatus = (dsActive, dsPending, dsPermanent, dsHistory);

  /// <summary>Flat row passed to the renderer; bindings copy from their own item types.</summary>
  TOBDDtcRowView = record
    Code: string;
    Description: string;
    Severity: TOBDDtcSeverity;
    Status: TOBDDtcStatus;
  end;

  TOBDDtcListRenderState = record
    Width, Height: Single;

    Rows: TArray<TOBDDtcRowView>;
    SelectedIndex: Integer;
    ScrollY: Single;

    RowHeight: Single;
    HeaderHeight: Single;
    ScrollbarWidth: Single;

    BackgroundColor: TAlphaColor;
    HeaderBackgroundColor: TAlphaColor;
    RowAlternateColor: TAlphaColor;
    BorderColor: TAlphaColor;
    TextColor: TAlphaColor;
    SelectionColor: TAlphaColor;
    SeverityInfoColor: TAlphaColor;
    SeverityWarningColor: TAlphaColor;
    SeverityCriticalColor: TAlphaColor;

    ShowHeader: Boolean;
    ShowAlternateRows: Boolean;
  end;

procedure RenderDtcList(const Canvas: ISkCanvas;
  const State: TOBDDtcListRenderState);

implementation

function ListAreaTop(const State: TOBDDtcListRenderState): Single;
begin
  if State.ShowHeader then Result := State.HeaderHeight else Result := 0;
end;

function ContentHeight(const State: TOBDDtcListRenderState): Single;
begin
  Result := Length(State.Rows) * State.RowHeight;
end;

function ListAreaHeight(const State: TOBDDtcListRenderState): Single;
begin
  Result := State.Height - ListAreaTop(State);
  if Result < 0 then Result := 0;
end;

function ColorForSeverity(const State: TOBDDtcListRenderState;
  S: TOBDDtcSeverity): TAlphaColor;
begin
  case S of
    dsInfo:     Result := State.SeverityInfoColor;
    dsWarning:  Result := State.SeverityWarningColor;
    dsCritical: Result := State.SeverityCriticalColor;
  else          Result := State.SeverityWarningColor;
  end;
end;

function StatusLabel(S: TOBDDtcStatus): string;
begin
  case S of
    dsActive:    Result := 'ACTIVE';
    dsPending:   Result := 'PENDING';
    dsPermanent: Result := 'PERM';
    dsHistory:   Result := 'HIST';
  else           Result := '';
  end;
end;

procedure DrawRow(const Canvas: ISkCanvas; const State: TOBDDtcListRenderState;
  const Row: TOBDDtcRowView; const RowRect: TRectF;
  const Selected, Alternate: Boolean; const Font, MonoFont: ISkFont);
var
  Paint: ISkPaint;
  StripeRect, StatusRect: TRectF;
  StatusText: string;
  StatusW: Single;
  CodeX, DescX: Single;
begin
  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Fill;
  if Selected then
    Paint.Color := State.SelectionColor
  else if Alternate and State.ShowAlternateRows then
    Paint.Color := State.RowAlternateColor
  else
    Paint.Color := State.BackgroundColor;
  Canvas.DrawRect(RowRect, Paint);

  StripeRect := RectF(RowRect.Left, RowRect.Top, RowRect.Left + 4, RowRect.Bottom);
  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := ColorForSeverity(State, Row.Severity);
  Canvas.DrawRect(StripeRect, Paint);

  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;
  Paint.Color := State.TextColor;

  CodeX := RowRect.Left + 12;
  Canvas.DrawSimpleText(Row.Code,
    CodeX,
    RowRect.Top + (RowRect.Bottom - RowRect.Top) / 2 + MonoFont.Size / 2 - 2,
    MonoFont, Paint);

  DescX := CodeX + MonoFont.MeasureText('XXXXXX', Paint) + 12;
  Canvas.DrawSimpleText(Row.Description,
    DescX,
    RowRect.Top + (RowRect.Bottom - RowRect.Top) / 2 + Font.Size / 2 - 2,
    Font, Paint);

  StatusText := StatusLabel(Row.Status);
  if StatusText <> '' then
  begin
    StatusW := Font.MeasureText(StatusText, Paint);
    StatusRect := RectF(RowRect.Right - StatusW - 16,
                        RowRect.Top + (RowRect.Bottom - RowRect.Top - 16) / 2,
                        RowRect.Right - 4,
                        RowRect.Top + (RowRect.Bottom - RowRect.Top + 16) / 2);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := State.HeaderBackgroundColor;
    Canvas.DrawRoundRect(StatusRect, 4, 4, Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := State.TextColor;
    Canvas.DrawSimpleText(StatusText,
      StatusRect.Left + 6,
      StatusRect.Top + (StatusRect.Bottom - StatusRect.Top) / 2 + Font.Size / 2 - 2,
      Font, Paint);
  end;
end;

procedure RenderDtcList(const Canvas: ISkCanvas;
  const State: TOBDDtcListRenderState);
var
  Bounds, RowRect, ScrollTrack, ScrollThumb: TRectF;
  Paint: ISkPaint;
  Font, MonoFont, HeaderFont: ISkFont;
  StartIdx, EndIdx, I: Integer;
  RowTop: Single;
  ContentH, ListH, MaxScroll: Single;
  ThumbH, ThumbY: Single;
begin
  Bounds := RectF(0, 0, State.Width, State.Height);

  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := State.BackgroundColor;
  Canvas.DrawRect(Bounds, Paint);

  Font := TSkFont.Create(TSkTypeface.MakeDefault, 12);
  MonoFont := TSkFont.Create(TSkTypeface.MakeFromName('Consolas',
    TSkFontStyle.Normal), 12);
  if MonoFont.Typeface = nil then
    MonoFont := TSkFont.Create(TSkTypeface.MakeDefault, 12);
  HeaderFont := TSkFont.Create(TSkTypeface.MakeDefault, 12);

  if State.ShowHeader then
  begin
    Paint := TSkPaint.Create;
    Paint.AntiAlias := False;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := State.HeaderBackgroundColor;
    Canvas.DrawRect(RectF(0, 0, State.Width, State.HeaderHeight), Paint);

    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Color := State.TextColor;
    Canvas.DrawSimpleText('Code',        12,
      State.HeaderHeight - 8, HeaderFont, Paint);
    Canvas.DrawSimpleText('Description', 12 + HeaderFont.MeasureText('XXXXXXXX', Paint),
      State.HeaderHeight - 8, HeaderFont, Paint);
    Canvas.DrawSimpleText('Status',      State.Width - 60,
      State.HeaderHeight - 8, HeaderFont, Paint);
  end;

  ListH := ListAreaHeight(State);
  ContentH := ContentHeight(State);
  if ListH <= 0 then Exit;

  StartIdx := Trunc(State.ScrollY / State.RowHeight);
  EndIdx := Trunc((State.ScrollY + ListH) / State.RowHeight) + 1;
  if StartIdx < 0 then StartIdx := 0;
  if EndIdx > High(State.Rows) then EndIdx := High(State.Rows);

  Canvas.Save;
  try
    Canvas.ClipRect(RectF(0, ListAreaTop(State), State.Width, State.Height));

    for I := StartIdx to EndIdx do
    begin
      RowTop := ListAreaTop(State) + (I * State.RowHeight) - State.ScrollY;
      RowRect := RectF(0, RowTop, State.Width - State.ScrollbarWidth,
                       RowTop + State.RowHeight);
      DrawRow(Canvas, State, State.Rows[I], RowRect,
              I = State.SelectedIndex, Odd(I), Font, MonoFont);
    end;
  finally
    Canvas.Restore;
  end;

  if ContentH > ListH then
  begin
    MaxScroll := ContentH - ListH;
    ScrollTrack := RectF(State.Width - State.ScrollbarWidth, ListAreaTop(State),
                         State.Width, State.Height);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := False;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := State.HeaderBackgroundColor;
    Canvas.DrawRect(ScrollTrack, Paint);

    ThumbH := System.Math.Max(20, (ListH / ContentH) * (ScrollTrack.Bottom - ScrollTrack.Top));
    ThumbY := ScrollTrack.Top +
      (State.ScrollY / System.Math.Max(1, MaxScroll)) *
      ((ScrollTrack.Bottom - ScrollTrack.Top) - ThumbH);
    ScrollThumb := RectF(ScrollTrack.Left + 2, ThumbY,
                         ScrollTrack.Right - 2, ThumbY + ThumbH);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := State.BorderColor;
    Canvas.DrawRoundRect(ScrollThumb, 2, 2, Paint);
  end;

  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.StrokeWidth := 1;
  Paint.Color := State.BorderColor;
  Canvas.DrawRect(Bounds, Paint);
end;

end.
