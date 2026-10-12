object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'OBD-Studio Personal'
  ClientHeight = 800
  ClientWidth = 1280
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 960
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object TitleBar: TOBDTitleBar
    Left = 0
    Top = 0
    Width = 1280
    Height = 44
    Buttons = <
      item
        Glyph = glMoon
        Hint = 'Switch to the dark theme'
        Name = 'theme'
      end
      item
        Glyph = glTablet
        Hint = 'Tablet density (larger controls for touch)'
        Name = 'density'
      end
      item
        Glyph = glHelp
        Hint = 'About OBD-Studio Personal'
        Name = 'help'
      end>
    QuickAccess = <
      item
        Glyph = glConnect
        Hint = 'Connect'
        Name = 'connect'
      end>
    Title = 'OBD-Studio Personal'
    AppInitials = 'OS'
    StatusCaption = 'Not connected'
    StatusVisible = True
    Align = alTop
    OnButtonClick = TitleBarButtonClick
    Theme = OBDTheme
    TabOrder = 0
  end
  object sdbMain: TOBDSidebar
    Left = 0
    Top = 44
    Width = 208
    Height = 728
    Items = <
      item
        Caption = 'Overview'
        Glyph = glVehicle
        Group = 'Vehicle'
      end
      item
        Caption = 'Trouble codes'
        Glyph = glCodes
        Tag = 1
        Group = 'Vehicle'
      end
      item
        Caption = 'Readiness'
        Glyph = glReadiness
        Tag = 2
        Group = 'Vehicle'
      end
      item
        Caption = 'Live data'
        Glyph = glLive
        Tag = 3
        Group = 'Vehicle'
      end
      item
        Caption = 'Dashboard'
        Glyph = glDashboard
        Tag = 4
        Group = 'Vehicle'
      end
      item
        Caption = 'Monitor tests'
        Glyph = glFlask
        Tag = 5
        Group = 'Tests'
      end
      item
        Caption = 'Vehicle info'
        Glyph = glVin
        Tag = 6
        Group = 'Tests'
      end
      item
        Caption = 'Fuel economy'
        Glyph = glFuel
        Tag = 7
        Group = 'Drive'
      end
      item
        Caption = 'Performance'
        Glyph = glDyno
        Tag = 8
        Group = 'Drive'
      end
      item
        Caption = 'EV battery'
        Glyph = glEvBattery
        Tag = 9
        Group = 'Drive'
      end
      item
        Caption = 'Adapter'
        Glyph = glAdapter
        Tag = 10
        Group = 'Tools'
      end
      item
        Caption = 'Recordings'
        Glyph = glReplay
        Tag = 11
        Group = 'Tools'
      end
      item
        Caption = 'Console'
        Glyph = glTerminal
        Tag = 12
        Group = 'Tools'
      end
      item
        Caption = 'Settings'
        Glyph = glGear
        Tag = 13
        Footer = True
      end>
    ItemIndex = 0
    OnChange = sdbMainChange
    Align = alLeft
    Theme = OBDTheme
    TabOrder = 1
  end
  object pnlPages: TPanel
    Left = 208
    Top = 44
    Width = 1072
    Height = 728
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
  end
  object sbMain: TOBDStatusBar
    Left = 0
    Top = 772
    Width = 1280
    Height = 28
    Panels = <
      item
        Kind = spkStatus
        Text = 'Not connected'
        Width = 140
        Hint = 'Adapter and connection'
      end
      item
        Width = 200
        Hint = 'Adapter'
      end
      item
        Width = 220
        Hint = 'Vehicle protocol'
      end
      item
        Glyph = glBattery
        Width = 80
        Hint = 'Battery voltage at the OBD socket'
      end
      item
        Kind = spkProgress
        Width = 220
        Visible = False
      end
      item
        Kind = spkLink
        Width = 160
        Alignment = paRight
        Hint = 'Show the trouble codes'
      end
      item
        Kind = spkLink
        Glyph = glRecord
        Width = 110
        Alignment = paRight
        StatusKind = skDanger
        Visible = False
        Hint = 'Show the recordings'
      end>
    OnPanelClick = sbMainPanelClick
    Align = alBottom
    Theme = OBDTheme
    TabOrder = 3
  end
  object OBDTheme: TOBDTheme
    Mode = tmWindows
    OnChange = OBDThemeChange
    Left = 1072
    Top = 200
  end
  object OBDHintStyle: TOBDHintStyle
    Theme = OBDTheme
    Left = 1160
    Top = 200
  end
  object OBDToasts: TOBDToastManager
    Theme = OBDTheme
    Left = 1072
    Top = 256
  end
  object dlgConfirm: TOBDDialog
    Theme = OBDTheme
    Left = 1160
    Top = 256
  end
  object tmrBattery: TTimer
    Enabled = False
    Interval = 15000
    OnTimer = tmrBatteryTimer
    Left = 1072
    Top = 312
  end
end
