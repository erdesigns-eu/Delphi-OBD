object frmPerformance: TfrmPerformance
  Left = 0
  Top = 0
  Width = 1208
  Height = 660
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  TabOrder = 0
  object pnlDrag: TPanel
    Left = 12
    Top = 12
    Width = 386
    Height = 636
    Align = alLeft
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object lapDrag: TOBDLapTimer
      Left = 0
      Top = 0
      Width = 386
      Height = 290
      Align = alTop
      Title = 'Drag run'
      TargetText = '1/4 mile '#183' 129 km/h '#183' best 17.48 s'
      TabOrder = 0
    end
    object pnlDragButtons: TPanel
      Left = 0
      Top = 290
      Width = 386
      Height = 48
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 1
      object btnStartRun: TOBDButton
        Left = 0
        Top = 8
        Width = 108
        Height = 36
        Caption = 'Start run'
        Glyph = glPlay
        OnClick = btnStartRunClick
        TabOrder = 0
      end
      object btnResetRun: TOBDButton
        Left = 116
        Top = 8
        Width = 90
        Height = 36
        Caption = 'Reset'
        Enabled = False
        Kind = bkSecondary
        Glyph = glRefresh
        OnClick = btnResetRunClick
        TabOrder = 1
      end
    end
    object pnlLiveTiles: TPanel
      Left = 0
      Top = 338
      Width = 386
      Height = 96
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 2
      object tileLiveSpeed: TOBDValueTile
        Left = 0
        Top = 0
        Width = 187
        Height = 88
        Alerts.Kinds = []
        Min = 0.000000000000000000
        Max = 160.000000000000000000
        Caption = 'Vehicle speed'
        Unit = 'km/h'
        ShowMinMax = False
        TabOrder = 0
      end
      object tileLiveRpm: TOBDValueTile
        Left = 199
        Top = 0
        Width = 187
        Height = 88
        Alerts.Kinds = []
        Min = 0.000000000000000000
        Max = 5000.000000000000000000
        Caption = 'Engine speed'
        Unit = 'rpm'
        ShowMinMax = False
        TabOrder = 1
      end
    end
    object chtSpeed: TOBDTrendChart
      Left = 0
      Top = 434
      Width = 386
      Height = 202
      Align = alClient
      TimeWindowSec = 20
      HistorySec = 120
      TabOrder = 3
    end
  end
  object pnlTrip: TPanel
    Left = 810
    Top = 12
    Width = 386
    Height = 636
    Align = alRight
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 2
    object cardTrip: TOBDCard
      Left = 0
      Top = 0
      Width = 386
      Height = 300
      Title = 'Trip'
      StatusEdge = ceAccent
      Align = alTop
      Padding.Left = 16
      Padding.Top = 12
      Padding.Right = 16
      Padding.Bottom = 12
      TabOrder = 0
      object tileTripAverage: TOBDValueTile
        Left = 16
        Top = 68
        Width = 171
        Height = 94
        Alerts.Kinds = []
        Min = 0.000000000000000000
        Max = 10.000000000000000000
        Caption = 'Average'
        Unit = 'l/100 km'
        Decimals = 1
        ShowMinMax = False
        ShowTrend = False
        TabOrder = 0
      end
      object tileTripNow: TOBDValueTile
        Left = 199
        Top = 68
        Width = 171
        Height = 94
        Alerts.Kinds = []
        Min = 0.000000000000000000
        Max = 10.000000000000000000
        Caption = 'Now'
        Unit = 'l/100 km'
        Decimals = 1
        ShowMinMax = False
        ShowTrend = False
        TabOrder = 1
      end
      object tileTripDistance: TOBDValueTile
        Left = 16
        Top = 174
        Width = 171
        Height = 94
        Alerts.Kinds = []
        Min = 0.000000000000000000
        Max = 200.000000000000000000
        Caption = 'Distance'
        Unit = 'km'
        Decimals = 1
        ShowMinMax = False
        ShowTrend = False
        TabOrder = 2
      end
      object tileTripCo2: TOBDValueTile
        Left = 199
        Top = 174
        Width = 171
        Height = 94
        Alerts.Kinds = []
        Min = 0.000000000000000000
        Max = 250.000000000000000000
        Caption = 'CO2 estimate'
        Unit = 'g/km'
        ShowMinMax = False
        ShowTrend = False
        TabOrder = 3
      end
    end
    object chtConsumption: TOBDTrendChart
      Left = 0
      Top = 316
      Width = 386
      Height = 320
      Align = alClient
      AlignWithMargins = True
      Margins.Left = 0
      Margins.Top = 16
      Margins.Right = 0
      Margins.Bottom = 0
      TimeWindowSec = 60
      HistorySec = 600
      TabOrder = 1
    end
  end
  object pnlBrake: TPanel
    Left = 410
    Top = 12
    Width = 388
    Height = 636
    Align = alClient
    AlignWithMargins = True
    Margins.Left = 12
    Margins.Top = 0
    Margins.Right = 12
    Margins.Bottom = 0
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    object cardBrake: TOBDCard
      Left = 0
      Top = 0
      Width = 364
      Height = 636
      Title = 'Brake test'
      StatusEdge = ceSuccess
      Align = alClient
      Padding.Left = 16
      Padding.Top = 12
      Padding.Right = 16
      Padding.Bottom = 12
      TabOrder = 0
      object gmBrake: TOBDGMeter
        Left = 32
        Top = 68
        Width = 300
        Height = 300
        Title = 'Friction circle'
        MaxG = 1.100000000000000000
        TrailLength = 60
        TabOrder = 0
      end
      object tileStopDistance: TOBDValueTile
        Left = 16
        Top = 392
        Width = 160
        Height = 88
        Alerts.Kinds = [alkHighWarning]
        Alerts.HighWarning = 40.000000000000000000
        Min = 0.000000000000000000
        Max = 60.000000000000000000
        Caption = '100-0 km/h'
        Unit = 'm'
        Decimals = 1
        ShowMinMax = False
        ShowTrend = False
        ShowSparkline = False
        TabOrder = 1
      end
      object tileStopTime: TOBDValueTile
        Left = 188
        Top = 392
        Width = 160
        Height = 88
        Alerts.Kinds = []
        Min = 0.000000000000000000
        Max = 6.000000000000000000
        Caption = 'Stop time'
        Unit = 's'
        Decimals = 2
        ShowMinMax = False
        ShowTrend = False
        ShowSparkline = False
        TabOrder = 2
      end
      object tilePeakG: TOBDValueTile
        Left = 16
        Top = 492
        Width = 160
        Height = 88
        Alerts.Kinds = []
        Min = 0.000000000000000000
        Max = 1.200000000000000000
        Caption = 'Peak'
        Unit = 'g'
        Decimals = 2
        ShowMinMax = False
        ShowTrend = False
        ShowSparkline = False
        TabOrder = 3
      end
      object tileMeanG: TOBDValueTile
        Left = 188
        Top = 492
        Width = 160
        Height = 88
        Alerts.Kinds = []
        Min = 0.000000000000000000
        Max = 1.200000000000000000
        Caption = 'Mean MFDD'
        Unit = 'g'
        Decimals = 2
        ShowMinMax = False
        ShowTrend = False
        ShowSparkline = False
        TabOrder = 4
      end
    end
  end
end
