object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'OBD Studio'
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
        Hint = 'Notifications'
        Name = 'notifications'
      end
      item
        Glyph = glHelp
        Hint = 'About OBD Studio'
        Name = 'help'
      end>
    QuickAccess = <
      item
        Glyph = glSave
        Hint = 'Save dashboard layout (Ctrl+S)'
        Name = 'save'
      end
      item
        Glyph = glRead
        Hint = 'Read trouble codes (F5)'
        Name = 'read'
      end
      item
        Glyph = glPrint
        Hint = 'Report'
        Name = 'report'
      end>
    Menu = mnuMain
    CommandStyle = csRibbon
    Ribbon = rbnMain
    Title = 'OBD Studio'
    Subtitle = 'Garage Peeters'
    AppInitials = 'OS'
    StatusCaption = 'Offline'
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
            Caption = 'Trouble codes'
            Items = <
              item
                Action = actReadCodes
                Caption = 'Read codes'
                Hint = 'Read stored, pending and permanent codes (F5)'
                Glyph = glRead
                Color = ricAccent
              end
              item
                Action = actClearCodes
                Caption = 'Clear codes'
                Hint = 'Clear codes and freeze frame, reset readiness'
                Glyph = glClear
                Color = ricDanger
              end
              item
                Caption = 'Freeze frame'
                Hint = 'Conditions when the code was set'
                Glyph = glSnapshot
                Size = risSmall
                Tag = 103
              end
              item
                Caption = 'Readiness'
                Hint = 'Emission monitor status'
                Glyph = glReadiness
                Size = risSmall
                Tag = 104
              end>
          end
          item
            Caption = 'Dashboard'
            Items = <
              item
                Action = actEditLayout
                Caption = 'Edit layout'
                Hint = 'Move, resize and add dashboard tiles'
                Glyph = glWrench
              end
              item
                Action = actSaveLayout
                Caption = 'Save layout'
                Hint = 'Save the dashboard layout (Ctrl+S)'
                Glyph = glSave
                Size = risSmall
              end
              item
                Action = actLoadLayout
                Caption = 'Load layout'
                Hint = 'Load the saved dashboard layout'
                Glyph = glOpen
                Size = risSmall
              end
              item
                Action = actResetLayout
                Caption = 'Reset layout'
                Hint = 'Back to the original dashboard'
                Glyph = glUndo
                Size = risSmall
              end>
          end
          item
            Caption = 'Report'
            Items = <
              item
                Action = actReport
                Caption = 'Report'
                Hint = 'Preview and print the diagnostic report'
                Glyph = glReport
              end>
          end>
      end
      item
        Caption = 'Live data'
        Groups = <
          item
            Caption = 'Recording'
            Items = <
              item
                Action = actPauseTrend
                Caption = 'Pause'
                Hint = 'Pause the live chart to scroll back'
                Glyph = glStop
              end
              item
                Action = actResetMinMax
                Caption = 'Reset min/max'
                Hint = 'Reset the minimum and maximum values'
                Glyph = glUndo
              end>
          end
          item
            Caption = 'Pages'
            Items = <
              item
                Caption = 'Live data'
                Hint = 'All parameters with a chart'
                Glyph = glLive
                Tag = 101
              end
              item
                Caption = 'Adapter log'
                Hint = 'Log and adapter terminal'
                Glyph = glCodes
                Tag = 106
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
          end
          item
            Caption = 'Commands'
            Items = <
              item
                Action = actMenuBar
                Caption = 'Menu bar'
                Hint = 'Use a classic menu bar instead of the ribbon'
                Glyph = glMore
              end>
          end>
      end>
    ContextualTabs = <
      item
        Caption = 'Dashboard layout'
        Visible = False
        Tabs = <
          item
            Caption = 'Layout'
            Groups = <
              item
                Caption = 'Layout'
                Items = <
                  item
                    Action = actEditLayout
                    Caption = 'Done'
                    Hint = 'Stop editing the dashboard'
                    Glyph = glCheck
                    Color = ricAccent
                  end
                  item
                    Action = actAddTile
                    Caption = 'Add tile'
                    Hint = 'Add a value tile in the first free cell'
                    Glyph = glPlus
                  end
                  item
                    Action = actResetLayout
                    Caption = 'Reset'
                    Hint = 'Back to the original dashboard'
                    Glyph = glUndo
                  end>
              end
              item
                Caption = 'File'
                Items = <
                  item
                    Action = actSaveLayout
                    Caption = 'Save layout'
                    Hint = 'Save the dashboard layout (Ctrl+S)'
                    Glyph = glSave
                  end
                  item
                    Action = actLoadLayout
                    Caption = 'Load layout'
                    Hint = 'Load the saved dashboard layout'
                    Glyph = glOpen
                  end>
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
        Text = 'Reading codes'
        Width = 200
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
          Caption = 'Dashboard'
          Icon = siDashboard
          Group = 'Diagnostics'
        end
        item
          Caption = 'Live data'
          Icon = siLiveData
          Tag = 1
          Group = 'Diagnostics'
        end
        item
          Caption = 'Trouble codes'
          Icon = siTroubleCodes
          BadgeKind = skDanger
          Tag = 2
          Group = 'Diagnostics'
        end
        item
          Caption = 'Freeze frame'
          Icon = siFreezeFrame
          Tag = 3
          Group = 'Diagnostics'
        end
        item
          Caption = 'Readiness'
          Icon = siReadiness
          Tag = 4
          Group = 'Diagnostics'
        end
        item
          Caption = 'Vehicle'
          Icon = siVehicle
          Tag = 5
          Group = 'Diagnostics'
        end
        item
          Caption = 'Log'
          Icon = siLog
          Tag = 6
          Group = 'Workshop'
        end
        item
          Caption = 'Settings'
          Icon = siSettings
          Tag = 7
          Footer = True
        end>
      ItemIndex = 0
      Title = 'OBD Studio'
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
      object pgDashboard: TPanel
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
        object tbDashboard: TOBDToolBar
          Left = 12
          Top = 12
          Width = 1184
          Height = 44
          Items = <
            item
              Action = actEditLayout
              Caption = 'Edit layout'
              Glyph = glWrench
              Hint = 'Move, resize and add dashboard tiles'
            end
            item
              Action = actAddTile
              Caption = 'Add tile'
              Glyph = glPlus
              Hint = 'Add a value tile in the first free cell'
            end
            item
              Kind = tkSeparator
            end
            item
              Action = actSaveLayout
              Caption = 'Save'
              Glyph = glSave
              Hint = 'Save the dashboard layout (Ctrl+S)'
            end
            item
              Action = actLoadLayout
              Caption = 'Load'
              Glyph = glOpen
              Hint = 'Load the saved dashboard layout'
            end
            item
              Action = actResetLayout
              Caption = 'Reset'
              Glyph = glUndo
              Hint = 'Back to the original dashboard'
            end
            item
              Kind = tkSpacer
            end
            item
              Action = actReadCodes
              Caption = 'Read codes'
              Glyph = glRead
              Color = ticAccent
              Hint = 'Read trouble codes (F5)'
            end
            item
              Action = actReport
              Caption = 'Report'
              Glyph = glPrint
              Hint = 'Preview and print the diagnostic report'
            end>
          Align = alTop
          Theme = OBDTheme
          TabOrder = 0
        end
        object dshMain: TOBDDashboard
          Left = 12
          Top = 56
          Width = 1184
          Height = 592
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
              Col = 4
              RowSpan = 2
            end
            item
              Control = barEngineLoad
              Col = 5
              RowSpan = 2
            end
            item
              Control = tileBattery
              Row = 2
            end
            item
              Control = tileEgrError
              Col = 1
              Row = 2
            end
            item
              Control = tileMaf
              Col = 2
              Row = 2
            end
            item
              Control = barBoost
              Col = 3
              Row = 2
            end
            item
              Control = tileFuel
              Col = 4
              Row = 2
            end
            item
              Control = lampMIL
              Col = 5
              Row = 2
            end
            item
              Control = chtDashboard
              Row = 3
              ColSpan = 4
            end
            item
              Control = mxTicker
              Col = 4
              Row = 3
              ColSpan = 2
            end>
          Columns = 6
          Rows = 4
          OnEmptyCellClick = dshMainEmptyCellClick
          OnEditModeChanged = dshMainEditModeChanged
          Align = alClient
          PopupMenu = pmDashboard
          Theme = OBDTheme
          TabOrder = 1
          object dlEngineSpeed: TOBDDialGauge
            Left = 8
            Top = 8
            Width = 381
            Height = 284
            Channel.PID = 12
            Channel.StaleAfterMs = 2000
            Alerts.Kinds = [alkHighWarning, alkHighAlarm]
            Alerts.HighWarning = 4200.000000000000000000
            Alerts.HighAlarm = 4800.000000000000000000
            Max = 5000.000000000000000000
            Caption = 'Engine speed'
            Unit = 'rpm'
            Theme = OBDTheme
            TabOrder = 0
          end
          object dlVehicleSpeed: TOBDDialGauge
            Left = 397
            Top = 8
            Width = 381
            Height = 284
            Channel.PID = 13
            Channel.StaleAfterMs = 2000
            Alerts.Kinds = []
            Max = 200.000000000000000000
            Caption = 'Vehicle speed'
            Unit = 'km/h'
            Theme = OBDTheme
            TabOrder = 1
          end
          object barCoolant: TOBDBarGauge
            Left = 786
            Top = 8
            Width = 191
            Height = 284
            Orientation = bgoVertical
            Channel.PID = 5
            Channel.StaleAfterMs = 2000
            Alerts.Kinds = [alkHighWarning, alkHighAlarm]
            Alerts.HighWarning = 105.000000000000000000
            Alerts.HighAlarm = 112.000000000000000000
            Min = 40.000000000000000000
            Max = 130.000000000000000000
            Caption = 'Coolant'
            Unit = #176'C'
            Theme = OBDTheme
            TabOrder = 2
          end
          object barEngineLoad: TOBDBarGauge
            Left = 985
            Top = 8
            Width = 191
            Height = 284
            Orientation = bgoVertical
            Channel.PID = 4
            Channel.StaleAfterMs = 2000
            Alerts.Kinds = []
            Max = 100.000000000000000000
            Caption = 'Engine load'
            Unit = '%'
            Theme = OBDTheme
            TabOrder = 3
          end
          object tileBattery: TOBDValueTile
            Left = 8
            Top = 300
            Width = 185
            Height = 138
            Channel.PID = 66
            Channel.StaleAfterMs = 2000
            Alerts.Kinds = [alkLowAlarm, alkLowWarning]
            Alerts.LowAlarm = 11.500000000000000000
            Alerts.LowWarning = 12.200000000000000000
            Min = 10.000000000000000000
            Max = 16.000000000000000000
            Caption = 'Battery'
            Unit = 'V'
            Decimals = 1
            Theme = OBDTheme
            TabOrder = 4
          end
          object tileEgrError: TOBDValueTile
            Left = 201
            Top = 300
            Width = 185
            Height = 138
            Channel.PID = 45
            Channel.StaleAfterMs = 2000
            Alerts.Kinds = [alkLowAlarm, alkLowWarning]
            Alerts.LowAlarm = -25.000000000000000000
            Alerts.LowWarning = -10.000000000000000000
            Min = -100.000000000000000000
            Max = 100.000000000000000000
            Caption = 'EGR error'
            Unit = '%'
            Decimals = 1
            Theme = OBDTheme
            TabOrder = 5
          end
          object tileMaf: TOBDValueTile
            Left = 394
            Top = 300
            Width = 185
            Height = 138
            Channel.PID = 16
            Channel.StaleAfterMs = 2000
            Alerts.Kinds = []
            Max = 120.000000000000000000
            Caption = 'Mass air flow'
            Unit = 'g/s'
            Decimals = 1
            Theme = OBDTheme
            TabOrder = 6
          end
          object barBoost: TOBDBarGauge
            Left = 587
            Top = 300
            Width = 185
            Height = 138
            Channel.PID = 11
            Channel.StaleAfterMs = 2000
            Alerts.Kinds = [alkHighWarning]
            Alerts.HighWarning = 230.000000000000000000
            Max = 255.000000000000000000
            Caption = 'Intake pressure'
            Unit = 'kPa'
            Theme = OBDTheme
            TabOrder = 7
          end
          object tileFuel: TOBDValueTile
            Left = 780
            Top = 300
            Width = 185
            Height = 138
            Channel.PID = 47
            Channel.StaleAfterMs = 2000
            Alerts.Kinds = [alkLowWarning]
            Alerts.LowWarning = 15.000000000000000000
            Max = 100.000000000000000000
            Caption = 'Fuel level'
            Unit = '%'
            Theme = OBDTheme
            TabOrder = 8
          end
          object lampMIL: TOBDStatusLamp
            Left = 973
            Top = 300
            Width = 185
            Height = 138
            Caption = 'Check engine'
            Theme = OBDTheme
            TabOrder = 9
          end
          object chtDashboard: TOBDTrendChart
            Left = 8
            Top = 446
            Width = 770
            Height = 138
            Channels = <
              item
                Caption = 'Engine speed'
                PID = 12
                Unit = 'rpm'
                Color = 1607920
                Max = 5000.000000000000000000
              end
              item
                Caption = 'Vehicle speed'
                PID = 13
                Unit = 'km/h'
                Color = 12100119
                Max = 200.000000000000000000
              end>
            TimeWindowSec = 60
            Theme = OBDTheme
            TabOrder = 10
          end
          object mxTicker: TOBDMatrixDisplay
            Left = 786
            Top = 446
            Width = 390
            Height = 138
            Preset = mxpTicker
            Text = 'OBD STUDIO   GOLF VII 1.6 TDI   SIMULATED ENGINE DATA'
            Scroll = mxsLeft
            OnPassComplete = mxTickerPassComplete
            Theme = OBDTheme
            TabOrder = 11
          end
        end
      end
      object pgLiveData: TPanel
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
        TabOrder = 2
        Visible = False
        object tbLive: TOBDToolBar
          Left = 12
          Top = 12
          Width = 1184
          Height = 44
          Items = <
            item
              Action = actPauseTrend
              Caption = 'Pause'
              Glyph = glStop
              Hint = 'Pause the chart to scroll back through the history'
            end
            item
              Action = actResetMinMax
              Caption = 'Reset min/max'
              Glyph = glUndo
              Hint = 'Reset the minimum and maximum values'
            end
            item
              Caption = 'Clear chart'
              Glyph = glClear
              Hint = 'Remove all samples from the chart'
              OnClick = tbLiveClearClick
            end>
          ShowSearch = True
          SearchHint = 'Filter parameters'
          Align = alTop
          Theme = OBDTheme
          TabOrder = 0
        end
        object grdLive: TOBDLiveDataGrid
          Left = 12
          Top = 56
          Width = 1184
          Height = 256
          StaleAfterMs = 2000
          ShowCheckboxes = True
          OnCheckChanged = grdLiveCheckChanged
          Align = alClient
          Theme = OBDTheme
          TabOrder = 1
        end
        object chtLive: TOBDTrendChart
          Left = 12
          Top = 312
          Width = 1184
          Height = 300
          TimeWindowSec = 30
          HistorySec = 300
          OnPauseChanged = chtLivePauseChanged
          Align = alBottom
          Theme = OBDTheme
          TabOrder = 2
        end
        object sbHistory: TOBDScrollBar
          Left = 12
          Top = 636
          Width = 1184
          Height = 12
          Kind = sbHorizontal
          OnChange = sbHistoryChange
          Align = alBottom
          Hint = 'Scroll back through the recorded history'
          Theme = OBDTheme
          TabOrder = 3
        end
      end
      object pgCodes: TPanel
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
        TabOrder = 3
        Visible = False
        object bnrCodes: TOBDBanner
          AlignWithMargins = True
          Left = 12
          Top = 12
          Width = 1184
          Height = 72
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 12
          Title = 'Codes not read yet'
          Text = 'Connect to the vehicle and press Read codes (F5).'
          Align = alTop
          Theme = OBDTheme
          TabOrder = 0
        end
        object pnlDtc: TOBDDtcPanel
          Left = 12
          Top = 96
          Width = 1184
          Height = 552
          OnReadCodes = pnlDtcReadCodes
          OnClearCodes = pnlDtcClearCodes
          Align = alClient
          Theme = OBDTheme
          TabOrder = 1
        end
      end
      object pgFreezeFrame: TPanel
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
        TabOrder = 4
        Visible = False
        object ffView: TOBDFreezeFrameView
          Left = 12
          Top = 12
          Width = 1184
          Height = 636
          RangeProfile = RangeProfile
          OnEditRanges = ffViewEditRanges
          Align = alClient
          Theme = OBDTheme
          TabOrder = 0
        end
      end
      object pgReadiness: TPanel
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
        TabOrder = 5
        Visible = False
        object rdPanel: TOBDReadinessPanel
          Left = 12
          Top = 12
          Width = 1184
          Height = 636
          InspectionRegime = irAPK
          Align = alClient
          Theme = OBDTheme
          TabOrder = 0
        end
      end
      object pgVehicle: TPanel
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
        TabOrder = 6
        Visible = False
        object vcVehicle: TOBDVehicleInfoCard
          AlignWithMargins = True
          Left = 12
          Top = 12
          Width = 1184
          Height = 230
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 12
          Align = alTop
          Theme = OBDTheme
          TabOrder = 0
        end
        object cdJob: TOBDCard
          Left = 12
          Top = 254
          Width = 1184
          Height = 394
          Title = 'Workshop job'
          StatusEdge = ceAccent
          Align = alClient
          Theme = OBDTheme
          TabOrder = 1
          object edCustomer: TOBDEdit
            Left = 20
            Top = 64
            Width = 300
            Height = 32
            Text = 'J. de Vries'
            TextHint = 'Customer name'
            Hint = 'Customer name'
            Theme = OBDTheme
            TabOrder = 0
          end
          object edPlate: TOBDEdit
            Left = 336
            Top = 64
            Width = 140
            Height = 32
            Text = 'GX-482-T'
            TextHint = 'Licence plate'
            Mono = True
            Hint = 'Licence plate'
            Theme = OBDTheme
            TabOrder = 1
          end
          object edVin: TOBDEdit
            Left = 20
            Top = 108
            Width = 300
            Height = 32
            MaxLength = 17
            TextHint = 'VIN (17 characters)'
            Mono = True
            Hint = 'Vehicle identification number'
            Theme = OBDTheme
            TabOrder = 2
          end
          object btnDecodeVin: TOBDButton
            Left = 336
            Top = 108
            Width = 140
            Height = 32
            Kind = bkSecondary
            Glyph = glVehicle
            Caption = 'Decode VIN'
            OnClick = btnDecodeVinClick
            Theme = OBDTheme
            TabOrder = 3
          end
          object cbJobType: TOBDComboBox
            Left = 20
            Top = 152
            Width = 300
            Height = 32
            Items.Strings = (
              'Diagnosis'
              'Periodic maintenance'
              'Inspection preparation (APK)'
              'Repair'
              'Road test')
            ItemIndex = 0
            Hint = 'Type of job'
            Theme = OBDTheme
            TabOrder = 4
          end
          object segFuel: TOBDSegmented
            Left = 336
            Top = 152
            Width = 320
            Height = 32
            Items.Strings = (
              'Petrol'
              'Diesel'
              'Hybrid'
              'EV')
            ItemIndex = 1
            Hint = 'Fuel type'
            Theme = OBDTheme
            TabOrder = 5
          end
          object chkRoadTest: TOBDCheckBox
            Left = 20
            Top = 200
            Width = 220
            Height = 24
            Checked = True
            Style = csSwitch
            Caption = 'Road test after repair'
            Theme = OBDTheme
            TabOrder = 6
          end
          object chkCustomerWaits: TOBDCheckBox
            Left = 256
            Top = 200
            Width = 220
            Height = 24
            Caption = 'Customer waits'
            Theme = OBDTheme
            TabOrder = 7
          end
          object rbPriorityNormal: TOBDRadioButton
            Left = 20
            Top = 236
            Width = 120
            Height = 24
            Checked = True
            GroupIndex = 1
            Caption = 'Normal'
            Theme = OBDTheme
            TabOrder = 8
          end
          object rbPriorityUrgent: TOBDRadioButton
            Left = 150
            Top = 236
            Width = 120
            Height = 24
            GroupIndex = 1
            Caption = 'Urgent'
            Theme = OBDTheme
            TabOrder = 9
          end
          object chpJobStatus: TOBDChip
            Left = 700
            Top = 66
            Width = 120
            Height = 24
            Caption = 'In progress'
            Kind = skAccent
            Theme = OBDTheme
          end
          object chpMil: TOBDChip
            Left = 828
            Top = 66
            Width = 90
            Height = 24
            Caption = 'MIL on'
            Kind = skDanger
            Theme = OBDTheme
          end
          object bdgCodes: TOBDBadge
            Left = 926
            Top = 68
            Width = 24
            Height = 20
            Kind = skDanger
            Hint = 'Trouble codes'
            Theme = OBDTheme
          end
          object rbrCharging: TOBDRangeBar
            Left = 700
            Top = 108
            Width = 360
            Height = 48
            Caption = 'Charging voltage'
            Value = 14.100000000000000000
            Min = 11.000000000000000000
            Max = 16.000000000000000000
            Low = 13.200000000000000000
            High = 14.800000000000000000
            Decimals = 1
            UnitText = 'V'
            Theme = OBDTheme
          end
          object pbJob: TOBDProgressBar
            Left = 700
            Top = 172
            Width = 460
            Height = 56
            Style = pbsSteps
            Steps = <
              item
                Caption = 'Check-in'
                State = stDone
              end
              item
                Caption = 'Connect'
                State = stActive
              end
              item
                Caption = 'Read codes'
                State = stPending
              end
              item
                Caption = 'Diagnose'
                State = stPending
              end
              item
                Caption = 'Repair'
                State = stPending
              end
              item
                Caption = 'Road test'
                State = stPending
              end
              item
                Caption = 'Hand-over'
                State = stPending
              end>
            Theme = OBDTheme
            TabOrder = 10
          end
          object btnStartScan: TOBDButton
            Left = 700
            Top = 244
            Width = 140
            Height = 32
            Glyph = glRead
            Caption = 'Start scan'
            OnClick = btnStartScanClick
            Theme = OBDTheme
            TabOrder = 11
          end
          object btnCancelJob: TOBDButton
            Left = 852
            Top = 244
            Width = 120
            Height = 32
            Kind = bkDangerOutline
            Caption = 'Cancel job'
            OnClick = btnCancelJobClick
            Theme = OBDTheme
            TabOrder = 12
          end
        end
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
        TabOrder = 7
        Visible = False
        object tabsLog: TOBDTabs
          Left = 12
          Top = 12
          Width = 1184
          Height = 40
          Tabs = <
            item
              Caption = 'Log'
              Glyph = glReport
              Closable = False
            end
            item
              Caption = 'Adapter terminal'
              Glyph = glEcu
              Closable = False
            end>
          OnChange = tabsLogChange
          Align = alTop
          Theme = OBDTheme
          TabOrder = 0
        end
        object lvLog: TOBDLogViewer
          Left = 12
          Top = 52
          Width = 1184
          Height = 596
          Align = alClient
          Theme = OBDTheme
          TabOrder = 1
        end
        object trmAdapter: TOBDTerminal
          Left = 12
          Top = 52
          Width = 1184
          Height = 596
          Align = alClient
          Visible = False
          Theme = OBDTheme
          TabOrder = 2
        end
      end
      object pgSettings: TPanel
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
        TabOrder = 8
        Visible = False
        object insSettings: TOBDInspector
          AlignWithMargins = True
          Left = 12
          Top = 12
          Width = 420
          Height = 636
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 12
          Margins.Bottom = 0
          OnPropertyChanged = insSettingsPropertyChanged
          Align = alLeft
          Theme = OBDTheme
          TabOrder = 0
        end
        object rngEditor: TOBDRangeEditor
          Left = 444
          Top = 12
          Width = 752
          Height = 636
          RangeProfile = RangeProfile
          OnSave = rngEditorSave
          OnCancel = rngEditorCancel
          Align = alClient
          Theme = OBDTheme
          TabOrder = 1
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
        Caption = 'Report'
        Glyph = glReport
        Page = rpReport
      end
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
    object rpReport: TOBDReportPreview
      Left = 240
      Top = 0
      Width = 1200
      Height = 900
      PageCount = 2
      OnPaintPage = rpReportPaintPage
      Visible = False
      Theme = OBDTheme
      TabOrder = 0
    end
    object cdAbout: TOBDCard
      Left = 240
      Top = 0
      Width = 1200
      Height = 900
      Title = 'About OBD Studio'
      Visible = False
      Theme = OBDTheme
      TabOrder = 1
      object bnrAbout: TOBDBanner
        AlignWithMargins = True
        Left = 20
        Top = 64
        Width = 1160
        Height = 96
        Margins.Left = 20
        Margins.Top = 64
        Margins.Right = 20
        Margins.Bottom = 20
        Title = 'OBD Studio example application'
        Text = 
          'Built with the Delphi-OBD components (ERDesigns). The data comes' +
          ' from a simulated Golf VII 1.6 TDI with a sticking EGR valve. Re' +
          'place the simulator with TOBDConnection / TOBDLiveData to diagno' +
          'se a real vehicle.'
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
  object RangeProfile: TOBDRangeProfile
    Left = 1232
    Top = 312
  end
  object alMain: TActionList
    Left = 1320
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
    object actReadCodes: TAction
      Category = 'Vehicle'
      Caption = 'Read codes'
      Hint = 'Read stored, pending and permanent codes'
      ShortCut = 116
      OnExecute = actReadCodesExecute
    end
    object actClearCodes: TAction
      Category = 'Vehicle'
      Caption = 'Clear codes...'
      Hint = 'Clear codes and freeze frame, reset readiness'
      OnExecute = actClearCodesExecute
    end
    object actEditLayout: TAction
      Category = 'Dashboard'
      Caption = 'Edit layout'
      Hint = 'Move, resize and add dashboard tiles'
      OnExecute = actEditLayoutExecute
    end
    object actAddTile: TAction
      Category = 'Dashboard'
      Caption = 'Add tile'
      Hint = 'Add a value tile in the first free cell'
      OnExecute = actAddTileExecute
    end
    object actSaveLayout: TAction
      Category = 'Dashboard'
      Caption = 'Save layout'
      Hint = 'Save the dashboard layout'
      ShortCut = 16467
      OnExecute = actSaveLayoutExecute
    end
    object actLoadLayout: TAction
      Category = 'Dashboard'
      Caption = 'Load layout'
      Hint = 'Load the saved dashboard layout'
      OnExecute = actLoadLayoutExecute
    end
    object actResetLayout: TAction
      Category = 'Dashboard'
      Caption = 'Reset layout...'
      Hint = 'Back to the original dashboard'
      OnExecute = actResetLayoutExecute
    end
    object actPauseTrend: TAction
      Category = 'Live data'
      Caption = 'Pause chart'
      Hint = 'Pause the live chart to scroll back'
      OnExecute = actPauseTrendExecute
    end
    object actResetMinMax: TAction
      Category = 'Live data'
      Caption = 'Reset min/max'
      Hint = 'Reset the minimum and maximum values'
      OnExecute = actResetMinMaxExecute
    end
    object actReport: TAction
      Category = 'File'
      Caption = 'Report...'
      Hint = 'Preview and print the diagnostic report'
      OnExecute = actReportExecute
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
    object actMenuBar: TAction
      Category = 'View'
      Caption = 'Menu bar instead of ribbon'
      Hint = 'Use a classic menu bar instead of the ribbon'
      OnExecute = actMenuBarExecute
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About OBD Studio'
      Hint = 'About OBD Studio'
      OnExecute = actAboutExecute
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Close OBD Studio'
      OnExecute = actExitExecute
    end
  end
  object mnuMain: TMainMenu
    AutoMerge = False
    Left = 1232
    Top = 368
    object miFile: TMenuItem
      Caption = '&File'
      object miReport: TMenuItem
        Action = actReport
      end
      object miFileSep: TMenuItem
        Caption = '-'
      end
      object miExit: TMenuItem
        Action = actExit
      end
    end
    object miVehicle: TMenuItem
      Caption = '&Vehicle'
      object miConnect: TMenuItem
        Action = actConnect
      end
      object miDisconnect: TMenuItem
        Action = actDisconnect
      end
      object miVehicleSep: TMenuItem
        Caption = '-'
      end
      object miReadCodes: TMenuItem
        Action = actReadCodes
      end
      object miClearCodes: TMenuItem
        Action = actClearCodes
      end
    end
    object miDashboard: TMenuItem
      Caption = '&Dashboard'
      object miEditLayout: TMenuItem
        Action = actEditLayout
      end
      object miAddTile: TMenuItem
        Action = actAddTile
      end
      object miDashboardSep: TMenuItem
        Caption = '-'
      end
      object miSaveLayout: TMenuItem
        Action = actSaveLayout
      end
      object miLoadLayout: TMenuItem
        Action = actLoadLayout
      end
      object miResetLayout: TMenuItem
        Action = actResetLayout
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
      end
    end
    object miHelp: TMenuItem
      Caption = '&Help'
      object miAbout: TMenuItem
        Action = actAbout
      end
    end
  end
  object pmDashboard: TOBDPopupMenu
    Theme = OBDTheme
    Left = 1320
    Top = 368
    object pmiEditLayout: TMenuItem
      Action = actEditLayout
    end
    object pmiAddTile: TMenuItem
      Action = actAddTile
    end
    object pmiSep: TMenuItem
      Caption = '-'
    end
    object pmiSaveLayout: TMenuItem
      Action = actSaveLayout
    end
    object pmiLoadLayout: TMenuItem
      Action = actLoadLayout
    end
    object pmiResetLayout: TMenuItem
      Action = actResetLayout
    end
  end
  object tmrConnect: TTimer
    Enabled = False
    Interval = 1200
    OnTimer = tmrConnectTimer
    Left = 1232
    Top = 424
  end
  object tmrSimulation: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrSimulationTimer
    Left = 1320
    Top = 424
  end
  object tmrReadCodes: TTimer
    Enabled = False
    Interval = 120
    OnTimer = tmrReadCodesTimer
    Left = 1232
    Top = 480
  end
end
