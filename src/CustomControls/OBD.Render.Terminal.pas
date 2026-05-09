//------------------------------------------------------------------------------
// UNIT           : OBD.Render.Terminal.pas
// CONTENTS       : Framework-neutral renderer for the live conversation viewer
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// COPYRIGHT      : © 2024-2026 Ernst Reidinga (ERDesigns)
//------------------------------------------------------------------------------
unit OBD.Render.Terminal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Math, System.Skia;

type
  TOBDTerminalDirection = (tdSent, tdReceived, tdInfo, tdError);

  TOBDTerminalLineView = record
    Direction: TOBDTerminalDirection;
    Text: string;
    Timestamp: TDateTime;
  end;

  TOBDTerminalRenderState = record
    Width, Height: Single;

    Lines: TArray<TOBDTerminalLineView>;
    ScrollY: Single;

    BackgroundColor: TAlphaColor;
    BorderColor: TAlphaColor;
    TextColor: TAlphaColor;
    TimestampColor: TAlphaColor;
    SentColor: TAlphaColor;
    ReceivedColor: TAlphaColor;
    InfoColor: TAlphaColor;
    ErrorColor: TAlphaColor;

    FontSize: Integer;
    Padding: Single;
    ScrollbarWidth: Single;
    ShowTimestamps: Boolean;
  end;

procedure RenderTerminal(const Canvas: ISkCanvas;
  const State: TOBDTerminalRenderState);

implementation

function ColorForDirection(const State: TOBDTerminalRenderState;
  D: TOBDTerminalDirection): TAlphaColor;
begin
  case D of
    tdSent:     Result := State.SentColor;
    tdReceived: Result := State.ReceivedColor;
    tdInfo:     Result := State.InfoColor;
    tdError:    Result := State.ErrorColor;
  else          Result := State.TextColor;
  end;
end;

function PrefixForDirection(D: TOBDTerminalDirection): string;
begin
  case D of
    tdSent:     Result := '> ';
    tdReceived: Result := '< ';
    tdInfo:     Result := '  ';
    tdError:    Result := '! ';
  else          Result := '  ';
  end;
end;

procedure RenderTerminal(const Canvas: ISkCanvas;
  const State: TOBDTerminalRenderState);
var
  Bounds, ScrollTrack, ScrollThumb: TRectF;
  Paint: ISkPaint;
  Font: ISkFont;
  StartIdx, EndIdx, I: Integer;
  Y: Single;
  L: TOBDTerminalLineView;
  Prefix, TS, FullText: string;
  TSWidth: Single;
  ContentH, ListH, MaxScroll: Single;
  ThumbH, ThumbY: Single;
  LineHeight: Integer;
  VisibleLineCount: Integer;
begin
  Bounds := RectF(0, 0, State.Width, State.Height);

  Paint := TSkPaint.Create;
  Paint.AntiAlias := False;
  Paint.Style := TSkPaintStyle.Fill;
  Paint.Color := State.BackgroundColor;
  Canvas.DrawRect(Bounds, Paint);

  Font := TSkFont.Create(TSkTypeface.MakeFromName('Consolas',
    TSkFontStyle.Normal), State.FontSize);
  if Font.Typeface = nil then
    Font := TSkFont.Create(TSkTypeface.MakeDefault, State.FontSize);

  LineHeight := State.FontSize + 4;
  if LineHeight < 1 then LineHeight := 1;
  VisibleLineCount := Trunc((State.Height - 2 * State.Padding) / LineHeight);
  if VisibleLineCount < 0 then VisibleLineCount := 0;

  StartIdx := Trunc(State.ScrollY / LineHeight);
  EndIdx := StartIdx + VisibleLineCount + 1;
  if StartIdx < 0 then StartIdx := 0;
  if EndIdx > High(State.Lines) then EndIdx := High(State.Lines);

  Canvas.Save;
  try
    Canvas.ClipRect(RectF(State.Padding, State.Padding,
                          State.Width - State.Padding - State.ScrollbarWidth,
                          State.Height - State.Padding));

    for I := StartIdx to EndIdx do
    begin
      L := State.Lines[I];
      Y := State.Padding + (I * LineHeight) - State.ScrollY + Font.Size;

      Prefix := PrefixForDirection(L.Direction);

      if State.ShowTimestamps then
      begin
        TS := FormatDateTime('hh:nn:ss.zzz', L.Timestamp) + '  ';
        Paint := TSkPaint.Create;
        Paint.AntiAlias := True;
        Paint.Color := State.TimestampColor;
        Canvas.DrawSimpleText(TS, State.Padding, Y, Font, Paint);
        TSWidth := Font.MeasureText(TS, Paint);
      end
      else
        TSWidth := 0;

      Paint := TSkPaint.Create;
      Paint.AntiAlias := True;
      Paint.Color := ColorForDirection(State, L.Direction);
      FullText := Prefix + L.Text;
      Canvas.DrawSimpleText(FullText,
        State.Padding + TSWidth, Y, Font, Paint);
    end;
  finally
    Canvas.Restore;
  end;

  ListH := VisibleLineCount * LineHeight;
  ContentH := Length(State.Lines) * LineHeight;
  MaxScroll := ContentH - ListH;
  if MaxScroll < 0 then MaxScroll := 0;
  if ContentH > ListH then
  begin
    ScrollTrack := RectF(State.Width - State.ScrollbarWidth, State.Padding,
                         State.Width - 1, State.Height - State.Padding);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := False;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := State.BorderColor;
    Canvas.DrawRect(ScrollTrack, Paint);

    ThumbH := System.Math.Max(20, (ListH / ContentH) *
      (ScrollTrack.Bottom - ScrollTrack.Top));
    ThumbY := ScrollTrack.Top +
      (State.ScrollY / System.Math.Max(1, MaxScroll)) *
      ((ScrollTrack.Bottom - ScrollTrack.Top) - ThumbH);
    ScrollThumb := RectF(ScrollTrack.Left + 1, ThumbY,
                         ScrollTrack.Right - 1, ThumbY + ThumbH);
    Paint := TSkPaint.Create;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill;
    Paint.Color := State.TextColor;
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
