object frmDashboard: TfrmDashboard
  Left = 0
  Top = 0
  Caption = 'OBD Studio - Dashboard'
  ClientHeight = 781
  ClientWidth = 1084
  Color = 16118769
  Constraints.MinHeight = 600
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object pnlToolbar: TPanel
    Left = 0
    Top = 0
    Width = 1084
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    Color = 16447992
    ParentBackground = False
    TabOrder = 0
    object lblTheme: TLabel
      Left = 12
      Top = 13
      Width = 37
      Height = 15
      Caption = 'Theme'
    end
    object lblUnits: TLabel
      Left = 232
      Top = 13
      Width = 28
      Height = 15
      Caption = 'Units'
    end
    object cbTheme: TComboBox
      Left = 60
      Top = 9
      Width = 156
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'ERDesigns light'
      OnChange = cbThemeChange
      Items.Strings = (
        'ERDesigns light'
        'ERDesigns dark'
        'Windows colours'
        'Follow system')
    end
    object cbUnits: TComboBox
      Left = 270
      Top = 9
      Width = 100
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Metric'
      OnChange = cbUnitsChange
      Items.Strings = (
        'Metric'
        'Imperial')
    end
    object chkEditLayout: TCheckBox
      Left = 392
      Top = 11
      Width = 100
      Height = 19
      Caption = 'Edit layout'
      TabOrder = 2
      OnClick = chkEditLayoutClick
    end
    object btnSaveLayout: TButton
      Left = 504
      Top = 8
      Width = 100
      Height = 26
      Caption = 'Save layout'
      TabOrder = 3
      OnClick = btnSaveLayoutClick
    end
    object btnLoadLayout: TButton
      Left = 612
      Top = 8
      Width = 100
      Height = 26
      Caption = 'Load layout'
      TabOrder = 4
      OnClick = btnLoadLayoutClick
    end
  end
  object OBDConnectionBar: TOBDConnectionBar
    Left = 0
    Top = 42
    Width = 1084
    Height = 36
    Theme = OBDTheme
    Battery.PID = 66
    Battery.StaleAfterMs = 2000
    LinkState = lnkConnected
    AdapterText = 'Simulator'
    ProtocolText = 'ISO 15765-4 CAN 11/500'
    VIN = 'WVWZZZ1KZAW000000'
    TabOrder = 1
  end
  object OBDDashboard: TOBDDashboard
    Left = 0
    Top = 78
    Width = 1084
    Height = 703
    Theme = OBDTheme
    Align = alClient
    TabOrder = 2
    Tiles = <
      item
        Control = dlEngineSpeed
        ColSpan = 2
        RowSpan = 2
      end
      item
        Control = dlVehicleSpeed
        Col = 2
        ColSpan = 2
        RowSpan = 2
      end
      item
        Control = barCoolant
        Row = 2
      end
      item
        Control = barEngineLoad
        Col = 1
        Row = 2
      end
      item
        Control = tileBattery
        Col = 2
        Row = 2
      end
      item
        Control = lampMIL
        Col = 3
        Row = 2
      end
      item
        Control = chtTrend
        Row = 3
        ColSpan = 4
        RowSpan = 2
      end
      item
        Control = mxTicker
        Row = 5
        ColSpan = 4
      end>
    Rows = 6
    object dlEngineSpeed: TOBDDialGauge
      Left = 8
      Top = 8
      Width = 530
      Height = 222
      Theme = OBDTheme
      TabOrder = 0
      Channel.PID = 12
      Channel.StaleAfterMs = 2000
      Alerts.Kinds = [alkHighWarning, alkHighAlarm]
      Alerts.HighWarning = 5500.000000000000000000
      Alerts.HighAlarm = 6500.000000000000000000
      Max = 7000.000000000000000000
      Caption = 'Engine speed'
      Unit = 'rpm'
    end
    object dlVehicleSpeed: TOBDDialGauge
      Left = 546
      Top = 8
      Width = 530
      Height = 222
      Theme = OBDTheme
      TabOrder = 1
      Channel.PID = 13
      Channel.StaleAfterMs = 2000
      Alerts.Kinds = []
      Max = 240.000000000000000000
      Caption = 'Vehicle speed'
      Unit = 'km/h'
    end
    object barCoolant: TOBDBarGauge
      Left = 8
      Top = 238
      Width = 261
      Height = 107
      Theme = OBDTheme
      TabOrder = 2
      Channel.PID = 5
      Channel.StaleAfterMs = 2000
      Alerts.Kinds = [alkHighWarning, alkHighAlarm]
      Alerts.HighWarning = 105.000000000000000000
      Alerts.HighAlarm = 115.000000000000000000
      Min = 40.000000000000000000
      Max = 130.000000000000000000
      Caption = 'Coolant'
      Unit = #176'C'
      Orientation = bgoVertical
    end
    object barEngineLoad: TOBDBarGauge
      Left = 277
      Top = 238
      Width = 261
      Height = 107
      Theme = OBDTheme
      TabOrder = 3
      Channel.PID = 4
      Channel.StaleAfterMs = 2000
      Alerts.Kinds = []
      Max = 100.000000000000000000
      Caption = 'Engine load'
      Unit = '%'
      Orientation = bgoVertical
    end
    object tileBattery: TOBDValueTile
      Left = 546
      Top = 238
      Width = 261
      Height = 107
      Theme = OBDTheme
      TabOrder = 4
      Channel.PID = 66
      Channel.StaleAfterMs = 2000
      Alerts.Kinds = [alkLowAlarm, alkLowWarning]
      Alerts.LowAlarm = 11.500000000000000000
      Alerts.LowWarning = 12.000000000000000000
      Min = 10.000000000000000000
      Max = 16.000000000000000000
      Caption = 'Battery'
      Unit = 'V'
      Decimals = 1
    end
    object lampMIL: TOBDStatusLamp
      Left = 815
      Top = 238
      Width = 261
      Height = 107
      Theme = OBDTheme
      TabOrder = 5
      State = lstOk
      Caption = 'Check engine'
    end
    object chtTrend: TOBDTrendChart
      Left = 8
      Top = 353
      Width = 1068
      Height = 222
      Theme = OBDTheme
      TabOrder = 6
      Channels = <
        item
          Caption = 'Engine speed'
          PID = 12
          Unit = 'rpm'
          Color = 1607920
          Max = 7000.000000000000000000
        end
        item
          Caption = 'Vehicle speed'
          PID = 13
          Unit = 'km/h'
          Color = 12100119
          Max = 240.000000000000000000
        end>
      TimeWindowSec = 30
    end
    object mxTicker: TOBDMatrixDisplay
      Left = 8
      Top = 583
      Width = 1068
      Height = 107
      Theme = OBDTheme
      TabOrder = 7
      Preset = mxpTicker
      Text = 'OBD STUDIO  -  SIMULATED ENGINE DATA'
      Columns = 96
      Scroll = mxsLeft
    end
  end
  object OBDTheme: TOBDTheme
    Mode = tmLight
    OnChange = OBDThemeChange
    Left = 960
    Top = 8
  end
  object tmrSimulation: TTimer
    Interval = 100
    OnTimer = tmrSimulationTimer
    Left = 1016
    Top = 8
  end
end
