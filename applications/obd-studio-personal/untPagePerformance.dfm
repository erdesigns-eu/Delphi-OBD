object frmPagePerformance: TfrmPagePerformance
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
      Width = 68
      Height = 15
      Caption = 'Performance'
    end
    object lblSubtitle: TLabel
      Left = 0
      Top = 31
      Width = 330
      Height = 15
      Caption = 'Acceleration runs and estimated power from OBD speed data'
    end
    object cmbRunType: TOBDComboBox
      Left = 652
      Top = 12
      Width = 160
      Height = 26
      Anchors = [akTop, akRight]
      ItemIndex = -1
      OnChange = cmbRunTypeChange
      TabOrder = 0
    end
    object btnArmRun: TOBDButton
      Left = 824
      Top = 8
      Width = 104
      Height = 36
      Anchors = [akTop, akRight]
      Caption = 'Arm run'
      Glyph = glPlay
      OnClick = btnArmRunClick
      TabOrder = 1
    end
    object btnCancelRun: TOBDButton
      Left = 940
      Top = 8
      Width = 108
      Height = 36
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      Enabled = False
      Kind = bkSecondary
      OnClick = btnCancelRunClick
      TabOrder = 2
    end
  end
  object bannerSafety: TOBDBanner
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
    Kind = bnWarning
    Title = 'Closed tracks only'
    Text = 'Use performance timing only on closed roads, tracks or a dyno.'
    TabOrder = 1
  end
  object bannerState: TOBDBanner
    Left = 12
    Top = 148
    Width = 1048
    Height = 56
    Align = alTop
    AlignWithMargins = True
    Margins.Left = 0
    Margins.Top = 8
    Margins.Right = 0
    Margins.Bottom = 0
    Title = 'Ready to arm'
    Text = 'Choose a run type and arm the timer.'
    TabOrder = 2
  end
  object pnlTiles: TPanel
    Left = 12
    Top = 216
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
    TabOrder = 3
    object tileSpeed: TOBDValueTile
      Left = 0
      Top = 0
      Width = 252
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 220.000000000000000000
      Caption = 'Speed'
      Unit = 'km/h'
      Decimals = 0
      ShowMinMax = False
      TabOrder = 0
    end
    object tileElapsed: TOBDValueTile
      Left = 264
      Top = 0
      Width = 252
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 40.000000000000000000
      Caption = 'Elapsed'
      Unit = 's'
      Decimals = 2
      ShowMinMax = False
      ShowTrend = False
      TabOrder = 1
    end
    object tileDistance: TOBDValueTile
      Left = 528
      Top = 0
      Width = 252
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 500.000000000000000000
      Caption = 'Distance'
      Unit = 'm'
      Decimals = 0
      ShowMinMax = False
      TabOrder = 2
    end
    object tilePower: TOBDValueTile
      Left = 796
      Top = 0
      Width = 252
      Height = 96
      Alerts.Kinds = []
      Min = 0.000000000000000000
      Max = 250.000000000000000000
      Caption = 'Power'
      Unit = 'kW'
      Decimals = 0
      ShowMinMax = False
      TabOrder = 3
    end
  end
  object pnlMain: TPanel
    Left = 12
    Top = 332
    Width = 1048
    Height = 230
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
    TabOrder = 4
    object cardRun: TOBDCard
      Left = 0
      Top = 0
      Width = 344
      Height = 230
      Align = alLeft
      Title = 'Current run'
      Padding.Left = 16
      Padding.Top = 8
      Padding.Right = 16
      Padding.Bottom = 12
      TabOrder = 0
      object kvRun: TOBDKeyValueList
        Left = 16
        Top = 64
        Width = 312
        Height = 150
        Align = alClient
        KeyWidth = 120
        TabOrder = 0
      end
    end
    object chtRun: TOBDTrendChart
      Left = 360
      Top = 0
      Width = 688
      Height = 230
      Align = alClient
      AlignWithMargins = True
      Margins.Left = 16
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      TimeWindowSec = 30
      HistorySec = 180
      TabOrder = 1
    end
  end
  object pnlBottom: TPanel
    Left = 12
    Top = 574
    Width = 1048
    Height = 142
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
    TabOrder = 5
    object cardSaved: TOBDCard
      Left = 0
      Top = 0
      Width = 620
      Height = 142
      Align = alClient
      Title = 'Saved runs'
      Padding.Left = 16
      Padding.Top = 8
      Padding.Right = 16
      Padding.Bottom = 12
      TabOrder = 0
      object kvSaved: TOBDKeyValueList
        Left = 16
        Top = 64
        Width = 588
        Height = 62
        Align = alClient
        KeyWidth = 220
        TabOrder = 0
      end
    end
    object cardSetup: TOBDCard
      Left = 636
      Top = 0
      Width = 412
      Height = 142
      Align = alRight
      AlignWithMargins = True
      Margins.Left = 16
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Title = 'Power estimate set-up'
      Padding.Left = 16
      Padding.Top = 8
      Padding.Right = 16
      Padding.Bottom = 12
      TabOrder = 1
      object lblMass: TLabel
        Left = 16
        Top = 64
        Width = 84
        Height = 15
        Caption = 'Mass with driver'
      end
      object lblDragArea: TLabel
        Left = 146
        Top = 64
        Width = 73
        Height = 15
        Caption = 'Drag area CdA'
      end
      object lblRolling: TLabel
        Left = 276
        Top = 64
        Width = 92
        Height = 15
        Caption = 'Rolling resistance'
      end
      object edtMass: TOBDEdit
        Left = 16
        Top = 84
        Width = 110
        Height = 26
        Text = '1500'
        NumbersOnly = True
        OnChange = edtSetupChange
        TabOrder = 0
      end
      object edtDragArea: TOBDEdit
        Left = 146
        Top = 84
        Width = 110
        Height = 26
        Text = '0.65'
        OnChange = edtSetupChange
        TabOrder = 1
      end
      object edtRolling: TOBDEdit
        Left = 276
        Top = 84
        Width = 110
        Height = 26
        Text = '0.012'
        OnChange = edtSetupChange
        TabOrder = 2
      end
    end
  end
end
