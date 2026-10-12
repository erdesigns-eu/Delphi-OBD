object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'OBD Studio Pro'
  ClientHeight = 900
  ClientWidth = 1440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object TitleBar: TOBDTitleBar
    Left = 0
    Top = 0
    Width = 1440
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
        Glyph = glBell
        Hint = 'Warnings in the session log'
        Name = 'notifications'
      end
      item
        Glyph = glHelp
        Hint = 'About OBD Studio Pro'
        Name = 'help'
      end>
    QuickAccess = <
      item
        Glyph = glConnect
        Hint = 'Connect or disconnect'
        Name = 'connect'
      end
      item
        Glyph = glLog
        Hint = 'Session log'
        Name = 'log'
      end>
    CommandStyle = csRibbon
    Ribbon = rbnMain
    Title = 'OBD Studio Pro'
    Subtitle = 'Garage Peeters'
    AppInitials = 'OP'
    StatusCaption = 'Offline'
    StatusVisible = True
    Align = alTop
    OnButtonClick = TitleBarButtonClick
    Theme = OBDTheme
    TabOrder = 0
  end
  object rbnMain: TOBDRibbon
    Left = 0
    Top = 44
    Width = 1440
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
                Hint = 'Connect to the vehicle'
                Glyph = glConnect
              end
              item
                Action = actDisconnect
                Caption = 'Disconnect'
                Hint = 'Close the adapter'
                Glyph = glDisconnect
              end>
          end
          item
            Caption = 'Modules'
            Items = <
              item
                Caption = 'ECU network'
                Hint = 'Scan all control units and show the network'
                Glyph = glNetwork
                Color = ricAccent
                Tag = 100
              end
              item
                Caption = 'Service functions'
                Hint = 'Service resets, regeneration and basic settings'
                Glyph = glWrench
                Tag = 101
              end>
          end
          item
            Caption = 'Coding and programming'
            Items = <
              item
                Caption = 'Coding'
                Hint = 'Long coding, adaptations and online coding'
                Glyph = glCoding
                Tag = 102
              end
              item
                Caption = 'Flash'
                Hint = 'Flash a control unit with a new software version'
                Glyph = glFlash
                Tag = 103
              end
              item
                Caption = 'Memory'
                Hint = 'Read and write EEPROM and flash memory'
                Glyph = glEeprom
                Size = risSmall
                Tag = 104
              end
              item
                Caption = 'Radio code'
                Hint = 'Calculate a radio unlock code'
                Glyph = glRadio
                Size = risSmall
                Tag = 105
              end
              item
                Caption = 'Keys'
                Hint = 'Keys, immobiliser and component protection'
                Glyph = glKey
                Size = risSmall
                Tag = 106
              end>
          end>
      end
      item
        Caption = 'Measure'
        Groups = <
          item
            Caption = 'Performance'
            Items = <
              item
                Caption = 'Dyno'
                Hint = 'Power and torque runs'
                Glyph = glDyno
                Tag = 107
              end
              item
                Caption = 'Performance'
                Hint = 'Acceleration runs, lap timer and G-meter'
                Glyph = glFinishFlag
                Tag = 108
              end>
          end
          item
            Caption = 'Measurement'
            Items = <
              item
                Caption = 'Measurement'
                Hint = 'Measuring blocks and signals with charts'
                Glyph = glScope
                Tag = 109
              end
              item
                Caption = 'Actuators and bus'
                Hint = 'Actuator tests, CAN trace, send and replay'
                Glyph = glCanBus
                Tag = 110
              end>
          end
          item
            Caption = 'Electric'
            Items = <
              item
                Caption = 'EV battery'
                Hint = 'High-voltage battery cells and health'
                Glyph = glEvBattery
                Tag = 111
              end>
          end
          item
            Caption = 'Session'
            Items = <
              item
                Caption = 'Session log'
                Hint = 'Everything the pages reported'
                Glyph = glLog
                Tag = 112
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
                Caption = 'Light'
                Hint = 'Light theme'
                Glyph = glSun
                Size = risSmall
              end
              item
                Action = actThemeDark
                Caption = 'Dark'
                Hint = 'Dark theme'
                Glyph = glMoon
                Size = risSmall
              end
              item
                Action = actThemeWindows
                Caption = 'Windows'
                Hint = 'Follow the Windows app mode'
                Glyph = glDesktop
                Size = risSmall
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
              end
              item
                Action = actDensityTablet
                Caption = 'Tablet'
                Hint = 'Larger controls for touch screens'
                Glyph = glTablet
              end>
          end
          item
            Caption = 'Units'
            Items = <
              item
                Action = actUnitsMetric
                Caption = 'Metric'
                Hint = 'km/h, '#176'C, kPa'
                Glyph = glGlobe
                Size = risSmall
              end
              item
                Action = actUnitsImperial
                Caption = 'Imperial'
                Hint = 'mph, '#176'F, psi'
                Glyph = glGlobe
                Size = risSmall
              end>
          end>
      end>
    Backstage = bsMain
    OnItemClick = rbnMainItemClick
    Theme = OBDTheme
    Align = alTop
    TabOrder = 1
  end
  object sbMain: TOBDStatusBar
    Left = 0
    Top = 872
    Width = 1440
    Height = 28
    Panels = <
      item
        Kind = spkStatus
        Text = 'Disconnected'
        Width = 140
        Hint = 'Click to connect'
      end
      item
        Width = 280
        Mono = True
      end
      item
        Kind = spkProgress
        Width = 240
        Visible = False
      end
      item
        Text = 'Simulated data'
        Glyph = glLive
        Width = 140
        Alignment = paRight
      end
      item
        Kind = spkLink
        Text = 'Metric'
        Width = 80
        Alignment = paRight
        Hint = 'Switch between metric and imperial units'
      end
      item
        Kind = spkLink
        Text = 'Light theme'
        Width = 100
        Alignment = paRight
        Hint = 'Switch between the light and dark theme'
      end>
    OnPanelClick = sbMainPanelClick
    Align = alBottom
    Theme = OBDTheme
    TabOrder = 3
  end
  object pnlBody: TPanel
    Left = 0
    Top = 176
    Width = 1440
    Height = 696
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 2
    object sdbMain: TOBDSidebar
      Left = 0
      Top = 0
      Width = 232
      Height = 696
      Items = <
        item
          Caption = 'ECU network'
          Glyph = glNetwork
          Group = 'Modules'
        end
        item
          Caption = 'Service functions'
          Glyph = glWrench
          Tag = 1
          Group = 'Modules'
        end
        item
          Caption = 'Coding'
          Glyph = glCoding
          Tag = 2
          Group = 'Coding and programming'
        end
        item
          Caption = 'Flash'
          Glyph = glFlash
          Tag = 3
          Group = 'Coding and programming'
        end
        item
          Caption = 'Memory'
          Glyph = glEeprom
          Tag = 4
          Group = 'Coding and programming'
        end
        item
          Caption = 'Radio code'
          Glyph = glRadio
          Tag = 5
          Group = 'Coding and programming'
        end
        item
          Caption = 'Keys and immobiliser'
          Glyph = glKey
          Tag = 6
          Group = 'Coding and programming'
        end
        item
          Caption = 'Dyno'
          Glyph = glDyno
          Tag = 7
          Group = 'Performance'
        end
        item
          Caption = 'Performance'
          Glyph = glFinishFlag
          Tag = 8
          Group = 'Performance'
        end
        item
          Caption = 'Measurement'
          Glyph = glScope
          Tag = 9
          Group = 'Measurement'
        end
        item
          Caption = 'Actuators and bus'
          Glyph = glCanBus
          Tag = 10
          Group = 'Measurement'
        end
        item
          Caption = 'EV battery'
          Glyph = glEvBattery
          Tag = 11
          Group = 'Measurement'
        end
        item
          Caption = 'Session log'
          Glyph = glLog
          Tag = 12
          Footer = True
        end>
      ItemIndex = 0
      Title = 'OBD Studio Pro'
      Subtitle = 'Garage Peeters'
      OnChange = sdbMainChange
      Align = alLeft
      Theme = OBDTheme
      TabOrder = 0
    end
    object pnlPages: TPanel
      Left = 232
      Top = 0
      Width = 1208
      Height = 696
      Align = alClient
      BevelOuter = bvNone
      ParentBackground = False
      ShowCaption = False
      TabOrder = 1
      object cbConnection: TOBDConnectionBar
        Left = 0
        Top = 0
        Width = 1208
        Height = 36
        Battery.PID = 66
        Battery.StaleAfterMs = 2000
        AdapterText = 'Simulator (ELM327 v2.3)'
        Align = alTop
        Theme = OBDTheme
        TabOrder = 0
      end
      object pgLog: TPanel
        Left = 0
        Top = 36
        Width = 1208
        Height = 660
        Align = alClient
        BevelOuter = bvNone
        Padding.Left = 12
        Padding.Top = 12
        Padding.Right = 12
        Padding.Bottom = 12
        ParentBackground = False
        ShowCaption = False
        TabOrder = 1
        Visible = False
        object lvLog: TOBDLogViewer
          Left = 12
          Top = 12
          Width = 1184
          Height = 636
          Align = alClient
          Theme = OBDTheme
          TabOrder = 0
        end
      end
    end
  end
  object bsMain: TOBDBackstage
    Left = 0
    Top = 0
    Width = 1440
    Height = 900
    Items = <
      item
        Caption = 'About'
        Glyph = glInfo
        Page = cdAbout
      end
      item
        Kind = bikSeparator
      end
      item
        Caption = 'Exit'
        Glyph = glClose
        Kind = bikCommand
        Placement = bipBottom
        OnClick = bsExitClick
      end>
    Align = alClient
    Visible = False
    Theme = OBDTheme
    TabOrder = 4
    object cdAbout: TOBDCard
      Left = 240
      Top = 0
      Width = 1200
      Height = 900
      Title = 'About OBD Studio Pro'
      Visible = False
      Theme = OBDTheme
      TabOrder = 0
      object bnrAbout: TOBDBanner
        AlignWithMargins = True
        Left = 20
        Top = 64
        Width = 1160
        Height = 112
        Margins.Left = 20
        Margins.Top = 64
        Margins.Right = 20
        Margins.Bottom = 20
        Title = 'OBD Studio Pro example application'
        Text = 
          'Built with the Delphi-OBD components (ERDesigns): the workshop f' +
          'unctions of a Pro edition on top of the free OBD Studio example.' +
          ' Every page is a frame of its own. The data comes from the same ' +
          'simulated Golf VII 1.6 TDI with a sticking EGR valve.'
        Align = alTop
        Theme = OBDTheme
        TabOrder = 0
      end
    end
  end
  object OBDTheme: TOBDTheme
    Mode = tmLight
    OnChange = OBDThemeChange
    Left = 1232
    Top = 200
  end
  object OBDHintStyle: TOBDHintStyle
    Theme = OBDTheme
    Left = 1320
    Top = 200
  end
  object OBDToasts: TOBDToastManager
    Theme = OBDTheme
    Left = 1232
    Top = 256
  end
  object dlgConfirm: TOBDDialog
    Theme = OBDTheme
    Left = 1320
    Top = 256
  end
  object alMain: TActionList
    Left = 1232
    Top = 312
    object actConnect: TAction
      Category = 'Vehicle'
      Caption = 'Connect'
      Hint = 'Connect to the vehicle'
      OnExecute = actConnectExecute
    end
    object actDisconnect: TAction
      Category = 'Vehicle'
      Caption = 'Disconnect'
      Hint = 'Close the adapter'
      OnExecute = actDisconnectExecute
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
      Caption = 'Desktop density'
      Hint = 'Compact controls for mouse and keyboard'
      OnExecute = actDensityExecute
    end
    object actDensityTablet: TAction
      Tag = 1
      Category = 'View'
      Caption = 'Tablet density'
      Hint = 'Larger controls for touch screens'
      OnExecute = actDensityExecute
    end
    object actUnitsMetric: TAction
      Category = 'View'
      Caption = 'Metric units'
      Hint = 'km/h, '#176'C, kPa'
      OnExecute = actUnitsExecute
    end
    object actUnitsImperial: TAction
      Tag = 1
      Category = 'View'
      Caption = 'Imperial units'
      Hint = 'mph, '#176'F, psi'
      OnExecute = actUnitsExecute
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About OBD Studio Pro'
      Hint = 'About OBD Studio Pro'
      OnExecute = actAboutExecute
    end
  end
  object tmrConnect: TTimer
    Enabled = False
    Interval = 1200
    OnTimer = tmrConnectTimer
    Left = 1320
    Top = 312
  end
  object tmrSimulation: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrSimulationTimer
    Left = 1232
    Top = 368
  end
end
