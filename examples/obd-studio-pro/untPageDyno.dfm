object frmDyno: TfrmDyno
  Left = 0
  Top = 0
  Width = 1208
  Height = 660
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  TabOrder = 0
  object pnlRun: TPanel
    Left = 12
    Top = 12
    Width = 1184
    Height = 84
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object cardRun: TOBDCard
      Left = 0
      Top = 0
      Width = 1184
      Height = 84
      ShowHeader = False
      StatusEdge = ceDanger
      Align = alClient
      Padding.Left = 16
      Padding.Top = 12
      Padding.Right = 16
      Padding.Bottom = 12
      TabOrder = 0
      object lblDynoTitle: TLabel
        Left = 24
        Top = 16
        Width = 33
        Height = 21
        Caption = 'Dyno'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblDynoState: TLabel
        Left = 24
        Top = 44
        Width = 47
        Height = 15
        Caption = 'OFFLINE'
      end
      object lblGearTitle: TLabel
        Left = 250
        Top = 17
        Width = 25
        Height = 15
        Caption = 'Gear'
      end
      object lblGear: TLabel
        Left = 250
        Top = 40
        Width = 10
        Height = 21
        Caption = '--'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblEngineTitle: TLabel
        Left = 345
        Top = 17
        Width = 38
        Height = 15
        Caption = 'Engine'
      end
      object lblEngine: TLabel
        Left = 345
        Top = 40
        Width = 47
        Height = 21
        Caption = '0 rpm'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblSpeedTitle: TLabel
        Left = 500
        Top = 17
        Width = 33
        Height = 15
        Caption = 'Speed'
      end
      object lblSpeed: TLabel
        Left = 500
        Top = 40
        Width = 55
        Height = 21
        Caption = '0 km/h'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblThrottleTitle: TLabel
        Left = 640
        Top = 17
        Width = 43
        Height = 15
        Caption = 'Throttle'
      end
      object lblThrottle: TLabel
        Left = 640
        Top = 40
        Width = 25
        Height = 21
        Caption = '0 %'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblBoostTitle: TLabel
        Left = 775
        Top = 17
        Width = 30
        Height = 15
        Caption = 'Boost'
      end
      object lblBoost: TLabel
        Left = 775
        Top = 40
        Width = 58
        Height = 21
        Caption = '0.00 bar'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnDynoStart: TOBDButton
        Left = 938
        Top = 24
        Width = 104
        Height = 36
        Anchors = [akTop, akRight]
        Caption = 'Start run'
        Glyph = glPlay
        OnClick = btnDynoStartClick
        TabOrder = 0
      end
      object btnDynoStop: TOBDButton
        Left = 1052
        Top = 24
        Width = 108
        Height = 36
        Anchors = [akTop, akRight]
        Caption = 'Stop run'
        Enabled = False
        Kind = bkDangerOutline
        Glyph = glStop
        OnClick = btnDynoStopClick
        TabOrder = 1
      end
    end
  end
  object pnlBody: TPanel
    Left = 12
    Top = 96
    Width = 1184
    Height = 552
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    object pnlRight: TPanel
      Left = 764
      Top = 0
      Width = 420
      Height = 552
      Align = alRight
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 1
      object pnlTiles: TPanel
        Left = 0
        Top = 0
        Width = 420
        Height = 180
        Align = alTop
        BevelOuter = bvNone
        ParentBackground = False
        ParentColor = True
        ShowCaption = False
        TabOrder = 0
        object tilePeakPower: TOBDValueTile
          Left = 0
          Top = 0
          Width = 204
          Height = 84
          Alerts.Kinds = []
          Min = 0.000000000000000000
          Max = 100.000000000000000000
          Caption = 'Peak power'
          Unit = 'kW'
          Decimals = 1
          ShowMinMax = False
          ShowTrend = False
          ShowSparkline = False
          TabOrder = 0
        end
        object tilePeakTorque: TOBDValueTile
          Left = 216
          Top = 0
          Width = 204
          Height = 84
          Alerts.Kinds = []
          Min = 0.000000000000000000
          Max = 320.000000000000000000
          Caption = 'Peak torque'
          Unit = 'Nm'
          ShowMinMax = False
          ShowTrend = False
          ShowSparkline = False
          TabOrder = 1
        end
        object tileWheelPower: TOBDValueTile
          Left = 0
          Top = 96
          Width = 204
          Height = 84
          Alerts.Kinds = []
          Min = 0.000000000000000000
          Max = 90.000000000000000000
          Caption = 'Wheel power'
          Unit = 'kW'
          Decimals = 1
          ShowMinMax = False
          ShowTrend = False
          ShowSparkline = False
          TabOrder = 2
        end
        object tileLoss: TOBDValueTile
          Left = 216
          Top = 96
          Width = 204
          Height = 84
          Alerts.Kinds = []
          Min = 0.000000000000000000
          Max = 30.000000000000000000
          Caption = 'Drivetrain loss'
          Unit = '%'
          Decimals = 1
          ShowMinMax = False
          ShowTrend = False
          ShowSparkline = False
          TabOrder = 3
        end
      end
      object insConditions: TOBDInspector
        Left = 0
        Top = 192
        Width = 420
        Height = 168
        Align = alTop
        AlignWithMargins = True
        Margins.Left = 0
        Margins.Top = 12
        Margins.Right = 0
        Margins.Bottom = 0
        ReadOnly = True
        SplitterPosition = 175
        TabOrder = 1
      end
      object insVehicle: TOBDInspector
        Left = 0
        Top = 372
        Width = 420
        Height = 180
        Align = alClient
        AlignWithMargins = True
        Margins.Left = 0
        Margins.Top = 12
        Margins.Right = 0
        Margins.Bottom = 0
        SplitterPosition = 175
        TabOrder = 2
      end
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 752
      Height = 552
      Align = alClient
      AlignWithMargins = True
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 12
      Margins.Bottom = 0
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 0
      object chtPower: TOBDXYChart
        Left = 0
        Top = 0
        Width = 740
        Height = 360
        Align = alTop
        Title = 'Power and torque'
        Subtitle = 'Run 3 '#183' SAE J1349 corrected'
        XTitle = 'Engine speed (rpm)'
        LeftTitle = 'kW'
        RightTitle = 'Nm'
        XMin = 1000.000000000000000000
        XMax = 5000.000000000000000000
        LeftMin = 0.000000000000000000
        LeftMax = 100.000000000000000000
        RightMin = 0.000000000000000000
        RightMax = 300.000000000000000000
        CursorX = 3000.000000000000000000
        TabOrder = 0
      end
      object insRuns: TOBDInspector
        Left = 0
        Top = 372
        Width = 740
        Height = 180
        Align = alClient
        AlignWithMargins = True
        Margins.Left = 0
        Margins.Top = 12
        Margins.Right = 0
        Margins.Bottom = 0
        ReadOnly = True
        SplitterPosition = 160
        TabOrder = 1
      end
    end
  end
  object dynoConditions: TOBDDynoConditions
    AmbientC = 18.000000000000000000
    PressureKPa = 101.200000000000000000
    RelHumidity = 54.000000000000000000
    Left = 912
    Top = 112
  end
  object powerCurve: TOBDPowerCurve
    Left = 1048
    Top = 112
  end
end
