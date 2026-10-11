//------------------------------------------------------------------------------
//  Tests.ERD.UI.ProData
//
//  Rendering and behaviour tests for TOBDHexView, TOBDBitEditor,
//  TOBDBusTrace, TOBDBusSend and TOBDReplayBar.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2024-2026 Ernst Reidinga (ERDesigns)
//  License     : see LICENSE
//
//  History     :
//    2026-10-11  ERD  Initial implementation for the OBD Studio Pro controls.
//------------------------------------------------------------------------------
unit Tests.ERD.UI.ProData;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  DUnitX.TestFramework,
  ERD.UI.Types,
  ERD.UI.HexView,
  ERD.UI.BitEditor,
  ERD.UI.BusTrace,
  ERD.UI.BusSend,
  ERD.UI.ReplayBar,
  Tests.ERD.UI.RenderHelpers;

type
  [TestFixture]
  TOBDHexViewTests = class
  strict private
    FView: TOBDHexView;
    FSelectionEventCount: Integer;
    procedure SelectionChanged(Sender: TObject; const ASelection: TOBDHexSelection);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsBytes;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDensityDiffers;
    [Test] procedure CompareCountsChangedBytes;
    [Test] procedure SelectionRaisesEvent;
  end;

  [TestFixture]
  TOBDBitEditorTests = class
  strict private
    FEditor: TOBDBitEditor;
    FChangedCount: Integer;
    procedure CodingChanged(Sender: TObject; AChangedCount: Integer);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsBits;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDensityDiffers;
    [Test] procedure ToggleBitUpdatesCoding;
    [Test] procedure LabelsStoreBitFields;
  end;

  [TestFixture]
  TOBDBusTraceTests = class
  strict private
    FTrace: TOBDBusTrace;
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsRows;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDensityDiffers;
    [Test] procedure CapacityTrimsFrames;
    [Test] procedure ChangedBytesAreRecorded;
  end;

  [TestFixture]
  TOBDBusSendTests = class
  strict private
    FSend: TOBDBusSend;
    FSendCount: Integer;
    FLastId: Cardinal;
    FLastDataLength: Integer;
    FPeriodicCount: Integer;
    procedure Sent(Sender: TObject; AId: Cardinal; AExtended: Boolean;
      const AData: TBytes);
    procedure PeriodicChanged(Sender: TObject; APeriodic: Boolean;
      APeriodMs: Integer);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsLine;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDensityDiffers;
    [Test] procedure ParsesIdAndData;
    [Test] procedure SendRaisesEvent;
    [Test] procedure PeriodicRaisesEvent;
  end;

  [TestFixture]
  TOBDReplayBarTests = class
  strict private
    FBar: TOBDReplayBar;
    FSeekCount: Integer;
    FSpeedCount: Integer;
    procedure Seeked(Sender: TObject; APositionMs: Int64);
    procedure SpeedChanged(Sender: TObject; ASpeed: Double);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;
    [Test] procedure PreviewDrawsBar;
    [Test] procedure EmptyStillDraws;
    [Test] procedure TabletDensityDiffers;
    [Test] procedure SeekClampsPosition;
    [Test] procedure SpeedRaisesEvent;
    [Test] procedure MarkersCanBeAdded;
  end;

implementation

function Bytes(const Values: array of Byte): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(Values));
  for I := 0 to Length(Values) - 1 do
    Result[I] := Values[I];
end;

{ TOBDHexViewTests }

procedure TOBDHexViewTests.Setup;
begin
  FView := TOBDHexView.Create(nil);
  FView.SetBounds(0, 0, 720, 360);
  FSelectionEventCount := 0;
end;

procedure TOBDHexViewTests.TearDown;
begin
  FreeAndNil(FView);
end;

procedure TOBDHexViewTests.SelectionChanged(Sender: TObject;
  const ASelection: TOBDHexSelection);
begin
  Inc(FSelectionEventCount);
end;

procedure TOBDHexViewTests.PreviewDrawsBytes;
begin
  FView.ForcePreview := True;
  Assert.IsTrue(InkRatio(FView) > 0.02, 'preview hex view is empty');
end;

procedure TOBDHexViewTests.EmptyStillDraws;
begin
  Assert.AreEqual(0, Length(FView.Data));
  Assert.IsTrue(InkRatio(FView) > 0.002, 'empty hex view draws nothing');
end;

procedure TOBDHexViewTests.TabletDensityDiffers;
var
  A, B: TBitmap;
begin
  FView.ForcePreview := True;
  FView.Density := dnDesktop;
  A := RenderControl(FView);
  FView.Density := dnTablet;
  B := RenderControl(FView);
  try
    Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet density did not alter layout');
  finally
    A.Free;
    B.Free;
  end;
end;

procedure TOBDHexViewTests.CompareCountsChangedBytes;
begin
  FView.Data := Bytes([$01, $02, $03, $04]);
  FView.CompareData := Bytes([$01, $FF, $03, $00]);
  Assert.IsTrue(FView.ByteChanged(1));
  Assert.IsTrue(FView.ByteChanged(3));
  Assert.AreEqual(2, FView.ChangedCount);
end;

procedure TOBDHexViewTests.SelectionRaisesEvent;
var
  Sel: TOBDHexSelection;
begin
  FView.Data := Bytes([$01, $02, $03, $04]);
  FView.OnSelectionChange := SelectionChanged;
  Sel.Offset := 1;
  Sel.Length := 2;
  FView.Selection := Sel;
  Assert.AreEqual(1, FSelectionEventCount);
  Assert.AreEqual(1, FView.Selection.Offset);
  Assert.AreEqual(2, FView.Selection.Length);
end;

{ TOBDBitEditorTests }

procedure TOBDBitEditorTests.Setup;
begin
  FEditor := TOBDBitEditor.Create(nil);
  FEditor.SetBounds(0, 0, 720, 420);
  FChangedCount := -1;
end;

procedure TOBDBitEditorTests.TearDown;
begin
  FreeAndNil(FEditor);
end;

procedure TOBDBitEditorTests.CodingChanged(Sender: TObject;
  AChangedCount: Integer);
begin
  FChangedCount := AChangedCount;
end;

procedure TOBDBitEditorTests.PreviewDrawsBits;
begin
  FEditor.ForcePreview := True;
  Assert.IsTrue(InkRatio(FEditor) > 0.02, 'preview bit editor is empty');
end;

procedure TOBDBitEditorTests.EmptyStillDraws;
begin
  Assert.AreEqual(0, Length(FEditor.Coding));
  Assert.IsTrue(InkRatio(FEditor) > 0.002, 'empty bit editor draws nothing');
end;

procedure TOBDBitEditorTests.TabletDensityDiffers;
var
  A, B: TBitmap;
begin
  FEditor.ForcePreview := True;
  FEditor.Density := dnDesktop;
  A := RenderControl(FEditor);
  FEditor.Density := dnTablet;
  B := RenderControl(FEditor);
  try
    Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet density did not alter layout');
  finally
    A.Free;
    B.Free;
  end;
end;

procedure TOBDBitEditorTests.ToggleBitUpdatesCoding;
begin
  FEditor.Coding := Bytes([$00]);
  FEditor.OriginalCoding := Bytes([$00]);
  FEditor.OnChange := CodingChanged;
  FEditor.ToggleBit(0, 2);
  Assert.AreEqual(Integer($04), Integer(FEditor.ByteValue(0)));
  Assert.AreEqual(1, FEditor.ChangedCount);
  Assert.AreEqual(1, FChangedCount);
end;

procedure TOBDBitEditorTests.LabelsStoreBitFields;
var
  Item: TOBDBitLabelItem;
begin
  Item := FEditor.AddOptionLabel(2, 0, 2, 'Coming home source',
    'Off|Light switch|Sensor');
  Assert.AreEqual(1, FEditor.Labels.Count);
  Assert.AreEqual('Light switch', Item.OptionCaption(1));
  Assert.AreEqual(Integer($03), Integer(Item.Mask));
end;

{ TOBDBusTraceTests }

procedure TOBDBusTraceTests.Setup;
begin
  FTrace := TOBDBusTrace.Create(nil);
  FTrace.SetBounds(0, 0, 780, 420);
end;

procedure TOBDBusTraceTests.TearDown;
begin
  FreeAndNil(FTrace);
end;

procedure TOBDBusTraceTests.PreviewDrawsRows;
begin
  FTrace.ForcePreview := True;
  Assert.IsTrue(InkRatio(FTrace) > 0.02, 'preview trace is empty');
end;

procedure TOBDBusTraceTests.EmptyStillDraws;
begin
  Assert.AreEqual(0, FTrace.FrameCount);
  Assert.IsTrue(InkRatio(FTrace) > 0.002, 'empty trace draws nothing');
end;

procedure TOBDBusTraceTests.TabletDensityDiffers;
var
  A, B: TBitmap;
begin
  FTrace.ForcePreview := True;
  FTrace.Density := dnDesktop;
  A := RenderControl(FTrace);
  FTrace.Density := dnTablet;
  B := RenderControl(FTrace);
  try
    Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet density did not alter layout');
  finally
    A.Free;
    B.Free;
  end;
end;

procedure TOBDBusTraceTests.CapacityTrimsFrames;
begin
  FTrace.Capacity := 2;
  FTrace.AddFrame(0, $100, False, Bytes([$01]), 'a');
  FTrace.AddFrame(1, $101, False, Bytes([$02]), 'b');
  FTrace.AddFrame(2, $102, False, Bytes([$03]), 'c');
  Assert.AreEqual(2, FTrace.FrameCount);
  Assert.AreEqual(Integer($101), Integer(FTrace.Frame(0).Id));
end;

procedure TOBDBusTraceTests.ChangedBytesAreRecorded;
begin
  FTrace.AddFrame(0, $280, False, Bytes([$01, $02]), 'first');
  FTrace.AddFrame(10, $280, False, Bytes([$01, $03]), 'second');
  Assert.AreEqual(2, FTrace.FrameCount);
  Assert.AreEqual(Integer(2), Integer(FTrace.Frame(1).ChangedMask));
end;

{ TOBDBusSendTests }

procedure TOBDBusSendTests.Setup;
begin
  FSend := TOBDBusSend.Create(nil);
  FSend.SetBounds(0, 0, 560, 128);
  FSendCount := 0;
  FLastId := 0;
  FLastDataLength := 0;
  FPeriodicCount := 0;
end;

procedure TOBDBusSendTests.TearDown;
begin
  FreeAndNil(FSend);
end;

procedure TOBDBusSendTests.Sent(Sender: TObject; AId: Cardinal;
  AExtended: Boolean; const AData: TBytes);
begin
  Inc(FSendCount);
  FLastId := AId;
  FLastDataLength := Length(AData);
end;

procedure TOBDBusSendTests.PeriodicChanged(Sender: TObject;
  APeriodic: Boolean; APeriodMs: Integer);
begin
  Inc(FPeriodicCount);
end;

procedure TOBDBusSendTests.PreviewDrawsLine;
begin
  FSend.ForcePreview := True;
  Assert.IsTrue(InkRatio(FSend) > 0.02, 'send line is empty');
end;

procedure TOBDBusSendTests.EmptyStillDraws;
begin
  FSend.CANIdText := '';
  FSend.DataText := '';
  Assert.IsTrue(InkRatio(FSend) > 0.002, 'empty send line draws nothing');
end;

procedure TOBDBusSendTests.TabletDensityDiffers;
var
  A, B: TBitmap;
begin
  FSend.Density := dnDesktop;
  A := RenderControl(FSend);
  FSend.Density := dnTablet;
  B := RenderControl(FSend);
  try
    Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet density did not alter layout');
  finally
    A.Free;
    B.Free;
  end;
end;

procedure TOBDBusSendTests.ParsesIdAndData;
var
  Id: Cardinal;
  Data: TBytes;
begin
  FSend.CANIdText := '7E0';
  FSend.DataText := '02 10 03';
  Assert.IsTrue(FSend.TryParseId(Id));
  Assert.IsTrue(FSend.TryParseData(Data));
  Assert.AreEqual(Integer($7E0), Integer(Id));
  Assert.AreEqual(3, Length(Data));
end;

procedure TOBDBusSendTests.SendRaisesEvent;
begin
  FSend.OnSend := Sent;
  FSend.Send;
  Assert.AreEqual(1, FSendCount);
  Assert.AreEqual(Integer($7E0), Integer(FLastId));
  Assert.AreEqual(8, FLastDataLength);
end;

procedure TOBDBusSendTests.PeriodicRaisesEvent;
begin
  FSend.OnPeriodicChange := PeriodicChanged;
  FSend.Periodic := True;
  Assert.AreEqual(1, FPeriodicCount);
end;

{ TOBDReplayBarTests }

procedure TOBDReplayBarTests.Setup;
begin
  FBar := TOBDReplayBar.Create(nil);
  FBar.SetBounds(0, 0, 900, 88);
  FSeekCount := 0;
  FSpeedCount := 0;
end;

procedure TOBDReplayBarTests.TearDown;
begin
  FreeAndNil(FBar);
end;

procedure TOBDReplayBarTests.Seeked(Sender: TObject; APositionMs: Int64);
begin
  Inc(FSeekCount);
end;

procedure TOBDReplayBarTests.SpeedChanged(Sender: TObject; ASpeed: Double);
begin
  Inc(FSpeedCount);
end;

procedure TOBDReplayBarTests.PreviewDrawsBar;
begin
  FBar.ForcePreview := True;
  Assert.IsTrue(InkRatio(FBar) > 0.02, 'replay bar is empty');
end;

procedure TOBDReplayBarTests.EmptyStillDraws;
begin
  FBar.FileName := '';
  FBar.InfoText := '';
  FBar.DurationMs := 0;
  Assert.IsTrue(InkRatio(FBar) > 0.002, 'empty replay bar draws nothing');
end;

procedure TOBDReplayBarTests.TabletDensityDiffers;
var
  A, B: TBitmap;
begin
  FBar.Density := dnDesktop;
  A := RenderControl(FBar);
  FBar.Density := dnTablet;
  B := RenderControl(FBar);
  try
    Assert.IsTrue(BitmapsDiffer(A, B, 20), 'tablet density did not alter layout');
  finally
    A.Free;
    B.Free;
  end;
end;

procedure TOBDReplayBarTests.SeekClampsPosition;
begin
  FBar.DurationMs := 1000;
  FBar.OnSeek := Seeked;
  FBar.Seek(5000);
  Assert.AreEqual(Int64(1000), FBar.PositionMs);
  Assert.AreEqual(1, FSeekCount);
end;

procedure TOBDReplayBarTests.SpeedRaisesEvent;
begin
  FBar.OnSpeedChange := SpeedChanged;
  FBar.Speed := 2.0;
  Assert.AreEqual(1, FSpeedCount);
  Assert.AreEqual(Double(2.0), FBar.Speed, 0.001);
end;

procedure TOBDReplayBarTests.MarkersCanBeAdded;
begin
  FBar.AddMarker(100, rpbNote, 'Note');
  FBar.AddMarker(200, rpbDtc, 'DTC');
  Assert.AreEqual(2, FBar.Markers.Count);
  FBar.ClearMarkers;
  Assert.AreEqual(0, FBar.Markers.Count);
end;

end.
