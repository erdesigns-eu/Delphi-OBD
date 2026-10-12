object frmMeasurement: TfrmMeasurement
  Left = 0
  Top = 0
  Width = 1208
  Height = 660
  Padding.Left = 12
  Padding.Top = 12
  Padding.Right = 12
  Padding.Bottom = 12
  TabOrder = 0
  object pnlBrowser: TPanel
    AlignWithMargins = True
    Left = 12
    Top = 12
    Width = 380
    Height = 636
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 12
    Margins.Bottom = 0
    Align = alLeft
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 0
    object sigBrowser: TOBDSignalBrowser
      Left = 0
      Top = 0
      Width = 380
      Height = 636
      Align = alClient
      OnCheckChange = sigBrowserCheckChange
      TabOrder = 0
    end
  end
  object pnlRight: TPanel
    Left = 404
    Top = 12
    Width = 792
    Height = 636
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    ShowCaption = False
    TabOrder = 1
    object sigGrid: TOBDSignalGrid
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 792
      Height = 236
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 12
      Align = alTop
      TabOrder = 0
    end
    object pnlLower: TPanel
      Left = 0
      Top = 248
      Width = 792
      Height = 388
      Align = alClient
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 1
      object mapInjection: TOBDMapGrid
        AlignWithMargins = True
        Left = 420
        Top = 0
        Width = 372
        Height = 388
        Margins.Left = 12
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        Title = 'Injection quantity'
        Subtitle = 'mg/stroke '#183' EDC17C64 '#183' 8 '#215' 8'
        AxisHint = 'Pedal % (rows) '#183' engine speed rpm (columns)'
        UnitText = 'mg'
        EditStep = 0.1
        TabOrder = 1
      end
      object chtSignals: TOBDTrendChart
        Left = 0
        Top = 0
        Width = 408
        Height = 388
        Align = alClient
        TimeWindowSec = 45
        HistorySec = 300
        ShowLegend = True
        ShowGrid = True
        TabOrder = 0
      end
    end
  end
end
