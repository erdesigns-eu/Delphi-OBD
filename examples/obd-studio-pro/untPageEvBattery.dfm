object frmEvBattery: TfrmEvBattery
  Left = 0
  Top = 0
  Width = 1208
  Height = 660
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  TabOrder = 0
  object pnlStatus: TPanel
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
    object cardStatus: TOBDCard
      Left = 0
      Top = 0
      Width = 1184
      Height = 84
      ShowHeader = False
      StatusEdge = ceSuccess
      Align = alClient
      Padding.Left = 16
      Padding.Top = 12
      Padding.Right = 16
      Padding.Bottom = 12
      TabOrder = 0
      object lblBatteryTitle: TLabel
        Left = 24
        Top = 16
        Width = 75
        Height = 21
        Caption = 'HV battery'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblBatteryState: TLabel
        Left = 24
        Top = 44
        Width = 82
        Height = 15
        Caption = 'OFFLINE SAMPLE'
      end
      object lblVehicleTitle: TLabel
        Left = 250
        Top = 17
        Width = 36
        Height = 15
        Caption = 'Vehicle'
      end
      object lblVehicle: TLabel
        Left = 250
        Top = 40
        Width = 95
        Height = 21
        Caption = 'BMW i3 (2017)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblPackTitle: TLabel
        Left = 430
        Top = 17
        Width = 35
        Height = 15
        Caption = 'Battery'
      end
      object lblPack: TLabel
        Left = 430
        Top = 40
        Width = 105
        Height = 21
        Caption = '94 Ah '#183' 33 kWh'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblMileageTitle: TLabel
        Left = 620
        Top = 17
        Width = 40
        Height = 15
        Caption = 'Mileage'
      end
      object lblMileage: TLabel
        Left = 620
        Top = 40
        Width = 72
        Height = 21
        Caption = '48 210 km'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblControlTitle: TLabel
        Left = 780
        Top = 17
        Width = 59
        Height = 15
        Caption = 'Control unit'
      end
      object lblControl: TLabel
        Left = 780
        Top = 40
        Width = 113
        Height = 21
        Caption = 'SME battery mgmt'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnExportReport: TOBDButton
        Left = 922
        Top = 24
        Width = 118
        Height = 36
        Anchors = [akTop, akRight]
        Caption = 'Export report'
        Kind = bkSecondary
        Glyph = glExport
        OnClick = btnExportReportClick
        TabOrder = 0
      end
      object btnBalanceCheck: TOBDButton
        Left = 1050
        Top = 24
        Width = 110
        Height = 36
        Anchors = [akTop, akRight]
        Caption = 'Start test'
        Glyph = glPlay
        OnClick = btnBalanceCheckClick
        TabOrder = 1
      end
    end
  end
  object pnlTiles: TPanel
    Left = 12
    Top = 108
    Width = 1184
    Height = 100
    Align = alTop
    AlignWithMargins = True
    Margins.Left = 0
    Margins.Top = 12
    Margins.Right = 0
    Margins.Bottom = 0
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    object tileSoH: TOBDValueTile
      Left = 0
      Top = 0
      Width = 187
      Height = 92
      Alerts.Kinds = [alkLowWarning]
      Alerts.LowWarning = 80.000000000000000000
      Min = 0.000000000000000000
      Max = 100.000000000000000000
      Caption = 'State of health'
      Unit = '%'
      ShowMinMax = False
      ShowTrend = False
      TabOrder = 0
    end
    object tileSoC: TOBDValueTile
      Left = 199
      Top = 0
      Width = 187
      Height = 92
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 100.000000000000000000
      Caption = 'State of charge'
      Unit = '%'
      ShowMinMax = False
      TabOrder = 1
    end
    object tilePackVoltage: TOBDValueTile
      Left = 398
      Top = 0
      Width = 187
      Height = 92
      Alerts.Kinds = []
      Min = 330.000000000000000000
      Max = 390.000000000000000000
      Caption = 'Pack voltage'
      Unit = 'V'
      Decimals = 1
      ShowMinMax = False
      TabOrder = 2
    end
    object tileCurrent: TOBDValueTile
      Left = 597
      Top = 0
      Width = 187
      Height = 92
      Alerts.Kinds = []
      Min = -120.000000000000000000
      Max = 120.000000000000000000
      Caption = 'Current'
      Unit = 'A'
      Decimals = 1
      ShowMinMax = False
      TabOrder = 3
    end
    object tileCellSpread: TOBDValueTile
      Left = 796
      Top = 0
      Width = 187
      Height = 92
      Alerts.Kinds = [alkHighWarning]
      Alerts.HighWarning = 60.000000000000000000
      Min = 0.000000000000000000
      Max = 120.000000000000000000
      Caption = 'Cell min/max'
      Unit = 'mV'
      ShowMinMax = False
      TabOrder = 4
    end
    object tileTemperature: TOBDValueTile
      Left = 997
      Top = 0
      Width = 187
      Height = 92
      Alerts.Kinds = [alkHighWarning]
      Alerts.HighWarning = 30.000000000000000000
      Min = 0.000000000000000000
      Max = 50.000000000000000000
      Caption = 'Temperature'
      Unit = #176'C'
      Decimals = 1
      ShowMinMax = False
      TabOrder = 5
    end
  end
  object pnlBody: TPanel
    Left = 12
    Top = 208
    Width = 1184
    Height = 440
    Align = alClient
    AlignWithMargins = True
    Margins.Left = 0
    Margins.Top = 12
    Margins.Right = 0
    Margins.Bottom = 0
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 2
    object pnlRight: TPanel
      Left = 744
      Top = 0
      Width = 440
      Height = 440
      Align = alRight
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 1
      object insSummary: TOBDInspector
        Left = 0
        Top = 0
        Width = 440
        Height = 172
        Align = alTop
        ReadOnly = True
        SplitterPosition = 180
        TabOrder = 0
      end
      object preBattery: TOBDPreconditionList
        Left = 0
        Top = 184
        Width = 440
        Height = 152
        Align = alTop
        AlignWithMargins = True
        Margins.Left = 0
        Margins.Top = 12
        Margins.Right = 0
        Margins.Bottom = 0
        Title = 'Battery test'
        FooterText = 'Reads every cell under a short load; about 4 minutes.'
        TabOrder = 1
      end
      object chtBattery: TOBDTrendChart
        Left = 0
        Top = 348
        Width = 440
        Height = 92
        Align = alClient
        AlignWithMargins = True
        Margins.Left = 0
        Margins.Top = 12
        Margins.Right = 0
        Margins.Bottom = 0
        TimeWindowSec = 60
        HistorySec = 300
        TabOrder = 2
      end
    end
    object cellMatrix: TOBDCellMatrix
      Left = 0
      Top = 0
      Width = 732
      Height = 440
      Align = alClient
      AlignWithMargins = True
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 12
      Margins.Bottom = 0
      Title = 'Cell voltages'
      ModuleCount = 8
      CellsPerModule = 12
      WarmTemp = 30.000000000000000000
      TabOrder = 0
    end
  end
end
