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
    Menu = mnuMain
    Ribbon = rbnMain
    Title = 'OBD-Studio Personal'
    AppInitials = 'OS'
    StatusCaption = 'Not connected'
    StatusVisible = True
    Align = alTop
    OnButtonClick = TitleBarButtonClick
    OnCommandStyleChange = TitleBarCommandStyleChange
    Theme = OBDTheme
    TabOrder = 0
  end
  object rbnMain: TOBDRibbon
    Left = 0
    Top = 44
    Width = 1280
    Height = 132
    Tabs = <
      item
        Caption = 'Home'
        Groups = <
          item
            Caption = 'Connection'
            Items = <
              item
                Action = actConnect
                Caption = 'Connect'
                Hint = 'Connect with the saved adapter'
                Glyph = glConnect
                Size = risLarge
                Color = ricAccent
              end
              item
                Action = actDisconnect
                Caption = 'Disconnect'
                Hint = 'Close the connection'
                Glyph = glDisconnect
                Size = risLarge
              end>
          end
          item
            Caption = 'Vehicle'
            Items = <
              item
                Action = actPageOverview
                Caption = 'Overview'
                Hint = 'Vehicle, codes and readiness at a glance'
                Glyph = glVehicle
                Size = risLarge
              end
              item
                Action = actPageCodes
                Caption = 'Trouble codes'
                Hint = 'Read and clear the trouble codes'
                Glyph = glCodes
                Size = risLarge
              end
              item
                Action = actPageReadiness
                Caption = 'Readiness'
                Hint = 'Readiness monitors and the inspection check'
                Glyph = glReadiness
                Size = risLarge
              end
              item
                Action = actPageLive
                Caption = 'Live data'
                Hint = 'Live sensor values with a chart'
                Glyph = glLive
                Size = risLarge
              end
              item
                Action = actPageDashboard
                Caption = 'Dashboard'
                Hint = 'Gauges for the most important values'
                Glyph = glDashboard
                Size = risLarge
              end>
          end>
      end
      item
        Caption = 'Tests and drive'
        Groups = <
          item
            Caption = 'Tests'
            Items = <
              item
                Action = actPageTests
                Caption = 'Monitor tests'
                Hint = 'On-board monitor test results'
                Glyph = glFlask
                Size = risLarge
              end
              item
                Action = actPageInfo
                Caption = 'Vehicle info'
                Hint = 'VIN, calibration IDs and ECUs'
                Glyph = glVin
                Size = risLarge
              end>
          end
          item
            Caption = 'Drive'
            Items = <
              item
                Action = actPageFuel
                Caption = 'Fuel economy'
                Hint = 'Consumption, trip costs and CO2'
                Glyph = glFuel
                Size = risLarge
              end
              item
                Action = actPagePerformance
                Caption = 'Performance'
                Hint = 'Acceleration and power runs'
                Glyph = glDyno
                Size = risLarge
              end
              item
                Action = actPageEV
                Caption = 'EV battery'
                Hint = 'State of charge and battery health'
                Glyph = glEvBattery
                Size = risLarge
              end>
          end>
      end
      item
        Caption = 'Tools'
        Groups = <
          item
            Caption = 'Tools'
            Items = <
              item
                Action = actPageAdapter
                Caption = 'Adapter'
                Hint = 'Choose and connect the adapter'
                Glyph = glAdapter
                Size = risLarge
              end
              item
                Action = actPageRecordings
                Caption = 'Recordings'
                Hint = 'Record and replay drives'
                Glyph = glReplay
                Size = risLarge
              end
              item
                Action = actPageConsole
                Caption = 'Console'
                Hint = 'Activity log and adapter terminal'
                Glyph = glTerminal
                Size = risLarge
              end>
          end
          item
            Caption = 'Settings'
            Items = <
              item
                Action = actPageSettings
                Caption = 'Settings'
                Hint = 'Language, appearance, units and data'
                Glyph = glGear
                Size = risLarge
              end>
          end>
      end
      item
        Caption = 'View'
        Groups = <
          item
            Caption = 'Theme'
            Items = <
              item
                Action = actThemeLight
                Caption = 'Light theme'
                Hint = 'Light theme'
                Glyph = glSun
              end
              item
                Action = actThemeDark
                Caption = 'Dark theme'
                Hint = 'Dark theme'
                Glyph = glMoon
              end
              item
                Action = actThemeWindows
                Caption = 'Follow Windows'
                Hint = 'Follow the Windows app mode'
                Glyph = glDesktop
              end>
          end
          item
            Caption = 'Density'
            Items = <
              item
                Action = actDensityDesktop
                Caption = 'Desktop'
                Hint = 'Compact controls for mouse and keyboard'
                Glyph = glDesktop
                Size = risLarge
              end
              item
                Action = actDensityTablet
                Caption = 'Tablet'
                Hint = 'Larger controls for touch screens'
                Glyph = glTablet
                Size = risLarge
              end>
          end
          item
            Caption = 'Units'
            Items = <
              item
                Action = actUnitsMetric
                Caption = 'Metric units'
                Hint = 'km, '#176'C, kPa, litres'
                Glyph = glGlobe
              end
              item
                Action = actUnitsImperial
                Caption = 'Imperial units'
                Hint = 'miles, '#176'F, psi, gallons'
                Glyph = glGlobe
              end>
          end
          item
            Caption = 'Commands'
            Items = <
              item
                Action = actMenuBar
                Caption = 'Menu bar'
                Hint = 'Show the commands in a classic menu bar'
                Glyph = glMore
                Size = risLarge
              end
              item
                Action = actRibbon
                Caption = 'Ribbon'
                Hint = 'Show the commands in a ribbon'
                Glyph = glToggle
                Size = risLarge
              end>
          end
          item
            Caption = 'Help'
            Items = <
              item
                Action = actAbout
                Caption = 'About OBD-Studio Personal'
                Hint = 'About OBD-Studio Personal'
                Glyph = glHelp
                Size = risLarge
              end>
          end>
      end>
    ShowFileButton = False
    SearchHint = 'Search commands  Alt+Q'
    Theme = OBDTheme
    TabOrder = 1
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
    TabOrder = 2
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
    TabOrder = 3
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
    TabOrder = 4
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
  object alMain: TActionList
    Left = 1160
    Top = 312
    object actConnect: TAction
      Category = 'Connection'
      Caption = 'Connect'
      Hint = 'Connect with the saved adapter'
      OnExecute = actConnectExecute
    end
    object actDisconnect: TAction
      Category = 'Connection'
      Caption = 'Disconnect'
      Hint = 'Close the connection'
      OnExecute = actDisconnectExecute
    end
    object actPageOverview: TAction
      Category = 'Pages'
      Caption = 'Overview'
      Hint = 'Vehicle, codes and readiness at a glance'
      OnExecute = actPageExecute
    end
    object actPageCodes: TAction
      Tag = 1
      Category = 'Pages'
      Caption = 'Trouble codes'
      Hint = 'Read and clear the trouble codes'
      OnExecute = actPageExecute
    end
    object actPageReadiness: TAction
      Tag = 2
      Category = 'Pages'
      Caption = 'Readiness'
      Hint = 'Readiness monitors and the inspection check'
      OnExecute = actPageExecute
    end
    object actPageLive: TAction
      Tag = 3
      Category = 'Pages'
      Caption = 'Live data'
      Hint = 'Live sensor values with a chart'
      OnExecute = actPageExecute
    end
    object actPageDashboard: TAction
      Tag = 4
      Category = 'Pages'
      Caption = 'Dashboard'
      Hint = 'Gauges for the most important values'
      OnExecute = actPageExecute
    end
    object actPageTests: TAction
      Tag = 5
      Category = 'Pages'
      Caption = 'Monitor tests'
      Hint = 'On-board monitor test results'
      OnExecute = actPageExecute
    end
    object actPageInfo: TAction
      Tag = 6
      Category = 'Pages'
      Caption = 'Vehicle info'
      Hint = 'VIN, calibration IDs and ECUs'
      OnExecute = actPageExecute
    end
    object actPageFuel: TAction
      Tag = 7
      Category = 'Pages'
      Caption = 'Fuel economy'
      Hint = 'Consumption, trip costs and CO2'
      OnExecute = actPageExecute
    end
    object actPagePerformance: TAction
      Tag = 8
      Category = 'Pages'
      Caption = 'Performance'
      Hint = 'Acceleration and power runs'
      OnExecute = actPageExecute
    end
    object actPageEV: TAction
      Tag = 9
      Category = 'Pages'
      Caption = 'EV battery'
      Hint = 'State of charge and battery health'
      OnExecute = actPageExecute
    end
    object actPageAdapter: TAction
      Tag = 10
      Category = 'Pages'
      Caption = 'Adapter'
      Hint = 'Choose and connect the adapter'
      OnExecute = actPageExecute
    end
    object actPageRecordings: TAction
      Tag = 11
      Category = 'Pages'
      Caption = 'Recordings'
      Hint = 'Record and replay drives'
      OnExecute = actPageExecute
    end
    object actPageConsole: TAction
      Tag = 12
      Category = 'Pages'
      Caption = 'Console'
      Hint = 'Activity log and adapter terminal'
      OnExecute = actPageExecute
    end
    object actPageSettings: TAction
      Tag = 13
      Category = 'Pages'
      Caption = 'Settings'
      Hint = 'Language, appearance, units and data'
      OnExecute = actPageExecute
    end
    object actThemeLight: TAction
      Category = 'View'
      Caption = 'Light theme'
      Hint = 'Light theme'
      OnExecute = actThemeExecute
    end
    object actThemeDark: TAction
      Tag = 1
      Category = 'View'
      Caption = 'Dark theme'
      Hint = 'Dark theme'
      OnExecute = actThemeExecute
    end
    object actThemeWindows: TAction
      Tag = 2
      Category = 'View'
      Caption = 'Follow Windows'
      Hint = 'Follow the Windows app mode'
      OnExecute = actThemeExecute
    end
    object actDensityDesktop: TAction
      Category = 'View'
      Caption = 'Desktop'
      Hint = 'Compact controls for mouse and keyboard'
      OnExecute = actDensityExecute
    end
    object actDensityTablet: TAction
      Tag = 1
      Category = 'View'
      Caption = 'Tablet'
      Hint = 'Larger controls for touch screens'
      OnExecute = actDensityExecute
    end
    object actUnitsMetric: TAction
      Category = 'View'
      Caption = 'Metric units'
      Hint = 'km, '#176'C, kPa, litres'
      OnExecute = actUnitsExecute
    end
    object actUnitsImperial: TAction
      Tag = 1
      Category = 'View'
      Caption = 'Imperial units'
      Hint = 'miles, '#176'F, psi, gallons'
      OnExecute = actUnitsExecute
    end
    object actMenuBar: TAction
      Category = 'View'
      Caption = 'Menu bar'
      Hint = 'Show the commands in a classic menu bar'
      OnExecute = actCommandsExecute
    end
    object actRibbon: TAction
      Tag = 1
      Category = 'View'
      Caption = 'Ribbon'
      Hint = 'Show the commands in a ribbon'
      OnExecute = actCommandsExecute
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About OBD-Studio Personal'
      Hint = 'About OBD-Studio Personal'
      OnExecute = actAboutExecute
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Close OBD-Studio Personal'
      OnExecute = actExitExecute
    end
  end
  object mnuMain: TMainMenu
    AutoMerge = False
    Left = 1072
    Top = 368
    object miFile: TMenuItem
      Caption = '&File'
      object miConnect: TMenuItem
        Action = actConnect
      end
      object miDisconnect: TMenuItem
        Action = actDisconnect
      end
      object miFileSep1: TMenuItem
        Caption = '-'
      end
      object miPageSettings: TMenuItem
        Action = actPageSettings
      end
      object miFileSep2: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Action = actExit
      end
    end
    object miVehicle: TMenuItem
      Caption = '&Vehicle'
      object miPageOverview: TMenuItem
        Action = actPageOverview
      end
      object miPageCodes: TMenuItem
        Action = actPageCodes
      end
      object miPageReadiness: TMenuItem
        Action = actPageReadiness
      end
      object miPageLive: TMenuItem
        Action = actPageLive
      end
      object miPageDashboard: TMenuItem
        Action = actPageDashboard
      end
    end
    object miTests: TMenuItem
      Caption = '&Tests'
      object miPageTests: TMenuItem
        Action = actPageTests
      end
      object miPageInfo: TMenuItem
        Action = actPageInfo
      end
    end
    object miDrive: TMenuItem
      Caption = '&Drive'
      object miPageFuel: TMenuItem
        Action = actPageFuel
      end
      object miPagePerformance: TMenuItem
        Action = actPagePerformance
      end
      object miPageEV: TMenuItem
        Action = actPageEV
      end
    end
    object miTools: TMenuItem
      Caption = 'T&ools'
      object miPageAdapter: TMenuItem
        Action = actPageAdapter
      end
      object miPageRecordings: TMenuItem
        Action = actPageRecordings
      end
      object miPageConsole: TMenuItem
        Action = actPageConsole
      end
    end
    object miView: TMenuItem
      Caption = 'Vie&w'
      object miThemeLight: TMenuItem
        Action = actThemeLight
        RadioItem = True
      end
      object miThemeDark: TMenuItem
        Action = actThemeDark
        RadioItem = True
      end
      object miThemeWindows: TMenuItem
        Action = actThemeWindows
        RadioItem = True
      end
      object miViewSep1: TMenuItem
        Caption = '-'
      end
      object miDensityDesktop: TMenuItem
        Action = actDensityDesktop
        GroupIndex = 1
        RadioItem = True
      end
      object miDensityTablet: TMenuItem
        Action = actDensityTablet
        GroupIndex = 1
        RadioItem = True
      end
      object miViewSep2: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miUnitsMetric: TMenuItem
        Action = actUnitsMetric
        GroupIndex = 2
        RadioItem = True
      end
      object miUnitsImperial: TMenuItem
        Action = actUnitsImperial
        GroupIndex = 2
        RadioItem = True
      end
      object miViewSep3: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object miMenuBar: TMenuItem
        Action = actMenuBar
        GroupIndex = 3
        RadioItem = True
      end
      object miRibbon: TMenuItem
        Action = actRibbon
        GroupIndex = 3
        RadioItem = True
      end
    end
    object miHelp: TMenuItem
      Caption = '&Help'
      object miAbout: TMenuItem
        Action = actAbout
      end
    end
  end
end
