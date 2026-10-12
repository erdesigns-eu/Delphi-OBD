object frmPageFuel: TfrmPageFuel
  Left = 0
  Top = 0
  Width = 1072
  Height = 728
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  TabOrder = 0
  object pnlHeader: TPanel
    Left = 12
    Top = 12
    Width = 1048
    Height = 64
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object lblTitle: TLabel
      Left = 0
      Top = 4
      Width = 76
      Height = 15
      Caption = 'Fuel economy'
    end
    object lblSubtitle: TLabel
      Left = 0
      Top = 31
      Width = 230
      Height = 15
      Caption = 'Live consumption, trip fuel use and cost'
    end
    object btnResetTrip: TOBDButton
      Left = 924
      Top = 12
      Width = 124
      Height = 36
      Anchors = [akTop, akRight]
      Caption = 'Reset trip'
      Kind = bkSecondary
      Glyph = glRefresh
      OnClick = btnResetTripClick
      TabOrder = 0
    end
  end
  object bannerState: TOBDBanner
    Left = 12
    Top = 84
    Width = 1048
    Height = 56
    Align = alTop
    AlignWithMargins = True
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 0
    Title = 'Connect to your car'
    Text = 'Fuel economy needs live data.'
    TabOrder = 1
  end
  object pnlTiles: TPanel
    Left = 12
    Top = 152
    Width = 1048
    Height = 104
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
    TabOrder = 2
    object tileAverage: TOBDValueTile
      Left = 0
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 20.000000000000000000
      Caption = 'Average'
      Unit = 'l/100 km'
      Decimals = 1
      ShowMinMax = False
      ShowTrend = False
      TabOrder = 0
    end
    object tileInstant: TOBDValueTile
      Left = 176
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 20.000000000000000000
      Caption = 'Right now'
      Unit = 'l/100 km'
      Decimals = 1
      ShowMinMax = False
      TabOrder = 1
    end
    object tileDistance: TOBDValueTile
      Left = 352
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 1000.000000000000000000
      Caption = 'Distance'
      Unit = 'km'
      Decimals = 1
      ShowMinMax = False
      ShowTrend = False
      TabOrder = 2
    end
    object tileFuelUsed: TOBDValueTile
      Left = 528
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 200.000000000000000000
      Caption = 'Fuel used'
      Unit = 'l'
      Decimals = 2
      ShowMinMax = False
      ShowTrend = False
      TabOrder = 3
    end
    object tileCost: TOBDValueTile
      Left = 704
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 500.000000000000000000
      Caption = 'Cost'
      Unit = 'EUR'
      Decimals = 2
      ShowMinMax = False
      ShowTrend = False
      TabOrder = 4
    end
    object tileCO2: TOBDValueTile
      Left = 884
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 400.000000000000000000
      Caption = 'CO2'
      Unit = 'g/km'
      Decimals = 0
      ShowMinMax = False
      ShowTrend = False
      TabOrder = 5
    end
  end
  object pnlMain: TPanel
    Left = 12
    Top = 268
    Width = 1048
    Height = 278
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
    TabOrder = 3
    object chtConsumption: TOBDTrendChart
      Left = 0
      Top = 0
      Width = 652
      Height = 278
      Align = alClient
      TimeWindowSec = 120
      HistorySec = 1800
      TabOrder = 0
    end
    object cardMeasure: TOBDCard
      Left = 668
      Top = 0
      Width = 380
      Height = 278
      Align = alRight
      AlignWithMargins = True
      Margins.Left = 16
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Title = 'How it is measured'
      Padding.Left = 16
      Padding.Top = 8
      Padding.Right = 16
      Padding.Bottom = 12
      TabOrder = 1
      object lblFuelPrice: TLabel
        Left = 16
        Top = 194
        Width = 50
        Height = 15
        Caption = 'Fuel price'
      end
      object lblCurrency: TLabel
        Left = 206
        Top = 194
        Width = 49
        Height = 15
        Caption = 'Currency'
      end
      object kvMeasure: TOBDKeyValueList
        Left = 16
        Top = 65
        Width = 348
        Height = 116
        ColumnCount = 1
        KeyWidth = 142
        TabOrder = 0
      end
      object edtFuelPrice: TOBDEdit
        Left = 16
        Top = 214
        Width = 170
        Height = 26
        Text = '1.95'
        OnChange = edtFuelPriceChange
        TabOrder = 1
      end
      object edtCurrency: TOBDEdit
        Left = 206
        Top = 214
        Width = 78
        Height = 26
        Text = 'EUR'
        MaxLength = 8
        CharCase = ecUpperCase
        OnChange = edtCurrencyChange
        TabOrder = 2
      end
    end
  end
  object histConsumption: TOBDHistogram
    Left = 12
    Top = 558
    Width = 1048
    Height = 158
    Align = alClient
    AlignWithMargins = True
    Margins.Left = 0
    Margins.Top = 12
    Margins.Right = 0
    Margins.Bottom = 0
    Title = 'Consumption histogram'
    UnitText = 'l/100 km'
    AutoRange = False
    Min = 0.000000000000000000
    Max = 20.000000000000000000
    BinCount = 18
    MaxSamples = 300
    NormalLow = 3.000000000000000000
    NormalHigh = 8.000000000000000000
    ShowNormalRange = True
    ShowMeanLine = True
    TabOrder = 4
  end
end
