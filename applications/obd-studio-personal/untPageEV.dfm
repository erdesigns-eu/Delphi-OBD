object frmPageEV: TfrmPageEV
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
      Width = 53
      Height = 15
      Caption = 'EV battery'
    end
    object lblSubtitle: TLabel
      Left = 0
      Top = 31
      Width = 245
      Height = 15
      Caption = 'High-voltage battery health and cell voltages'
    end
    object lblVendor: TLabel
      Left = 616
      Top = 15
      Width = 72
      Height = 15
      Anchors = [akTop, akRight]
      Caption = 'Vendor profile'
    end
    object cmbVendor: TOBDComboBox
      Left = 700
      Top = 12
      Width = 172
      Height = 26
      Anchors = [akTop, akRight]
      ItemIndex = -1
      OnChange = cmbVendorChange
      TabOrder = 0
    end
    object btnReadAgain: TOBDButton
      Left = 888
      Top = 8
      Width = 160
      Height = 36
      Anchors = [akTop, akRight]
      Caption = 'Read again'
      Glyph = glRead
      OnClick = btnReadAgainClick
      TabOrder = 1
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
    Text = 'EV battery data is read while connected.'
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
    object tileSOH: TOBDValueTile
      Left = 0
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = [alkLowWarning]
      Alerts.LowWarning = 80.000000000000000000
      Min = 0.000000000000000000
      Max = 100.000000000000000000
      Caption = 'State of health'
      Unit = '%'
      Decimals = 0
      ShowMinMax = False
      ShowTrend = False
      TabOrder = 0
    end
    object tileSOC: TOBDValueTile
      Left = 176
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 100.000000000000000000
      Caption = 'State of charge'
      Unit = '%'
      Decimals = 0
      ShowMinMax = False
      TabOrder = 1
    end
    object tilePackVoltage: TOBDValueTile
      Left = 352
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 900.000000000000000000
      Caption = 'Pack voltage'
      Unit = 'V'
      Decimals = 1
      ShowMinMax = False
      TabOrder = 2
    end
    object tilePackCurrent: TOBDValueTile
      Left = 528
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = []
      Min = -400.000000000000000000
      Max = 400.000000000000000000
      Caption = 'Current'
      Unit = 'A'
      Decimals = 1
      ShowMinMax = False
      TabOrder = 3
    end
    object tileTemperature: TOBDValueTile
      Left = 704
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = [alkHighWarning]
      Alerts.HighWarning = 45.000000000000000000
      Min = -20.000000000000000000
      Max = 70.000000000000000000
      Caption = 'Temperature'
      Unit = #176'C'
      Decimals = 1
      ShowMinMax = False
      TabOrder = 4
    end
    object tileCellDelta: TOBDValueTile
      Left = 884
      Top = 0
      Width = 164
      Height = 96
      Alerts.Kinds = [alkHighWarning]
      Alerts.HighWarning = 60.000000000000000000
      Min = 0.000000000000000000
      Max = 200.000000000000000000
      Caption = 'Cell delta'
      Unit = 'mV'
      Decimals = 0
      ShowMinMax = False
      TabOrder = 5
    end
  end
  object pnlBody: TPanel
    Left = 12
    Top = 268
    Width = 1048
    Height = 448
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
    TabOrder = 3
    object cellMatrix: TOBDCellMatrix
      Left = 0
      Top = 0
      Width = 652
      Height = 448
      Align = alClient
      Title = 'Cell voltages'
      ModuleCount = 7
      CellsPerModule = 14
      WarmTemp = 35.000000000000000000
      TabOrder = 0
    end
    object pnlRight: TPanel
      Left = 668
      Top = 0
      Width = 380
      Height = 448
      Align = alRight
      AlignWithMargins = True
      Margins.Left = 16
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 1
      object cardDetails: TOBDCard
        Left = 0
        Top = 0
        Width = 380
        Height = 250
        Align = alTop
        Title = 'Battery details'
        Padding.Left = 16
        Padding.Top = 8
        Padding.Right = 16
        Padding.Bottom = 12
        TabOrder = 0
        object kvDetails: TOBDKeyValueList
          Left = 16
          Top = 64
          Width = 348
          Height = 170
          Align = alClient
          KeyWidth = 142
          TabOrder = 0
        end
      end
      object chtHealth: TOBDTrendChart
        Left = 0
        Top = 266
        Width = 380
        Height = 182
        Align = alClient
        AlignWithMargins = True
        Margins.Left = 0
        Margins.Top = 16
        Margins.Right = 0
        Margins.Bottom = 0
        TimeWindowSec = 300
        HistorySec = 3600
        TabOrder = 1
      end
    end
  end
end
